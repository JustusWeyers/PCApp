#' classGroup
#'
#' @description Definition of group class
#'
#' @importFrom shiny tagList uiOutput fluidRow moduleServer reactiveValues NS
#' @importFrom shiny observeEvent renderUI fileInput actionButton selectInput
#' @importFrom shinydashboard box
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom methods new slot
#' @importFrom purrr pmap map_vec
#' @importFrom grDevices colors
#' @importFrom gplots col2hex
#' @importFrom colourpicker colourInput
#'
#' @noRd

# Class definition

setClass("Group",
         slots = c(
           key = "numeric",
           name = "character",
           dtype = "character",
           gparam = "list"
         ),
         prototype = list(
           key = NA_integer_,
           name = NA_character_,
           dtype = NA_character_,
           gparam = list(
             color = "grey"
           )
         )
)

# Methods

## Group UI

setGeneric("groupUI", function(obj) standardGeneric("groupUI"))

setMethod(groupUI,
          definition = function (obj) {
            ui = function(id = obj@name) {
              ns <- shiny::NS(id)
              shiny::tagList(

                ####

                # The group box ui
                uiOutput(ns("ui_groupbox"))

                ####

              )
            }
            return(ui)
          })

## Group server

setGeneric("groupServer", function(obj, r, import_server) standardGeneric("groupServer"))

setMethod("groupServer",
          definition = function(obj, r, import_server) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              ##################
              ### Connstants ###
              ##################

              RANDOMADDRESS = random_address()


              #################
              ### Functions ###
              #################

              get_data_objects = function() {
                print("Get data objects")
                # Fetch primarytable (pt)
                pt = get.table(shiny::isolate(r$db), "primary_table")
                pt = pt[pt$dgroup == obj@key,]

                # Build new data objects (do) from pt
                do = lapply(pt$key, function(key) {
                  methods::new(
                    Class = obj@dtype,
                    key = key,
                    name = pt[pt$key == key, "name"],
                    dtype = pt[pt$key == key, "dtype"],
                    dgroup = pt[pt$key == key, "dgroup"],
                    rparam = as.list(jsonlite::fromJSON(pt[pt$key == key, "rparam"])),
                    dparam = as.list(jsonlite::fromJSON(pt[pt$key == key, "dparam"]))
                  )
                })
                # Set names of new do
                do = stats::setNames(do, pt$name)
                # Return fresh data objects
                return(do)
              }

              ## Create data object from file input
              new_do = function(name, size, type, datapath, dtype, dgroup) {
                # Extract name for display
                display_name = tools::file_path_sans_ext(name)
                # Combine working name as pair of displayName and datatype
                n = stringr::str_replace_all(display_name, "[^[:alnum:]]", "")
                working_name = paste0(n, "_", dtype, "_", dgroup)
                # File extension
                ext = tools::file_ext(name)

                dparam = list(
                  fileext = ext, filename = name, filepath = datapath,
                  displayname = display_name, filetype = type, filesize = size
                )

                # Create new upload data object
                do = methods::new(
                  Class = dtype, name = working_name, dtype = dtype,
                  dgroup = dgroup, dparam = dparam
                )
                return(do)
              }

              ##########################
              ### REACTIVE FUNCTIONS ###
              ##########################

              get_gparam = function() {
                dgt = get.table(shiny::isolate(r$db), "datagroup_table")
                gparam = as.list(jsonlite::fromJSON(dgt[dgt$key == obj@key, "gparam"]))
                return(gparam)
              }

              set_gparam = function(new_gparam) {
                db = shiny::isolate(r$db)
                key = shiny::isolate(group_server$obj@key)
                # Discard null elements from new gparams
                old_gparam = get_gparam()
                new_gparam = purrr::discard(new_gparam, is.null)
                # Replace new elements in old gparams
                old_gparam[names(new_gparam)] <- new_gparam
                # Write gparams
                st = toString(jsonlite::toJSON(old_gparam))
                st = gsub("'", "", st)
                change.tablevalue(db, "datagroup_table", key, "gparam", st)
              }

              fileinput = reactive({
                input[[paste0(RANDOMADDRESS, "_fileinput")]]
              })

              readmethod = reactive({
                input[[paste0(RANDOMADDRESS, "_readmethod")]]
              })

              read_parameter = reactive({
                optional_fun_param(readmethod())
              })

              group_options = reactive({
                new(group_server$obj@dtype)@cc
              })

              color_picker = reactive({
                input[[paste0(RANDOMADDRESS, "_colorpicker")]]
              })

              read_parameter_inputs = reactive({
                input_elements = read_parameter()
                l = lapply(names(input_elements), function(n) input[[paste(RANDOMADDRESS, n, sep = "-")]])
                l = setNames(l, names(input_elements))
                return(l)
              })

              group_option_inputs = reactive({
                input_elements = group_options()
                l = lapply(names(input_elements), function(n) input[[paste(RANDOMADDRESS, n, sep = "-")]])
                l = setNames(l, names(input_elements))
                return(l)
              })

              ########################
              ### Server Functions ###
              ########################

              # 1. Set up reactive values
              group_server = reactiveValues(
                obj = obj,
                new_data_objects = get_data_objects(),
                delete_data = NULL,
                color = get_gparam()[["color"]],
                group_options = NULL,
                read_options = NULL,
                group_options = NULL,
                columnnames = c()
              )

              # 2. Observe file input
              shiny::observeEvent(
                eventExpr = fileinput(),
                handlerExpr = {
                  # Fetch upload
                  upload = fileinput()
                  upload$datapath <- gsub("\\\\", "/", upload$datapath)
                  # Check if upload is not null
                  if (any(!is.null(upload))) {
                    # Create new data objects via import function
                    new_data_objects = purrr::pmap(
                      upload, new_do, dtype = group_server$obj@dtype,
                      dgroup = group_server$obj@key
                    )
                    # Get the names of the newly created data objects
                    nms = purrr::map_vec(new_data_objects, function(do) do@name)
                    # Add data objects to group data
                    group_server$new_data_objects[nms] <- new_data_objects
                  }
              })

              # 3. Observe/handle new data objects
              shiny::observeEvent(
                eventExpr = group_server$new_data_objects,
                handlerExpr = {

                  print("Observed group_server$new_data_objects")

                  lapply(group_server$new_data_objects, function(do) {
                    # Create primary table entry
                    pt_entry = data.frame(
                      name = do@name,
                      dtype = do@dtype,
                      dgroup = do@dgroup,
                      rparam = toString(jsonlite::toJSON(do@rparam)),
                      dparam = toString(jsonlite::toJSON(do@dparam))
                    )
                    # Append or replace
                    if (do@name %in% group_server$pt$name) {
                      key = group_server$pt[group_server$pt$name == do@name, "key"]
                      replace.by.primary_key(r$db, "primary_table", key, pt_entry)
                    } else if (is.na(do@key)) {
                      DBI::dbAppendTable(r$db@con, "primary_table", pt_entry)
                    } else {
                      replace.by.primary_key(r$db, "primary_table", do@key, pt_entry)
                    }
                  })

                  print("1. Get Group")
                  group_server$new_data_objects <- get_data_objects()

                  print("2. Lapply")
                  # Call group servers
                  lapply(group_server$new_data_objects, function(o) {
                    boxServer(o, r = r, group_server = group_server)
                  })

                  print("3. Check")

                  nms <- names(group_server$new_data_objects)
                  group_server$data_objects[nms] <- group_server$new_data_objects
                  group_server$new_data_objects <- NULL

              })

              ## 4. Observe colorpicker
              observeEvent(color_picker(), {
                if (!is.null(is.null(color_picker()))) {
                  gparam = get_gparam()
                  gparam[["color"]] <- color_picker()
                  set_gparam(gparam)
                }
                group_server$color <- get_gparam()[["color"]]
              })

              ## 5. Observe read parameter inputs
              observeEvent(read_parameter_inputs(), {
                if (!is.null(read_parameter_inputs())) {
                  group_server$read_options <- read_parameter_inputs()
                  group_server$columnnames <- NULL
                  rp = group_server$read_options
                  rp = rp[!sapply(rp,is.null)]
                  gparam = get_gparam()
                  gparam[names(rp)] <- rp
                  set_gparam(gparam)
                }
              })

              ## 5. Observe group parameter inputs
              observeEvent(group_option_inputs(), {
                if (!is.null(group_option_inputs())) {
                  group_server$group_options <- group_option_inputs()
                  gparam = get_gparam()
                  gparam[names(group_server$group_options)] <- group_server$group_options
                  set_gparam(gparam)
                }
              })

              ## 6. Observe read method input
              observeEvent(readmethod(), {
                gparam = get_gparam()
                gparam[["readmethod"]] = readmethod()
                set_gparam(gparam)
              })

              ## 7. Observe delete button
              shiny::observeEvent(
                eventExpr = input[[paste0(RANDOMADDRESS, "_deletebutton")]],
                handlerExpr = {
                  print(paste("Delete", group_server$obj@name))
                  # Add data to delete queue
                  import_server$delete_groups <- append(import_server$delete, group_server$obj@name)
                }
              )

              ## 8. Delete data objects
              shiny::observeEvent(
                eventExpr = group_server$delete_data,
                handlerExpr = {
                  print("Delete Data")
                  # Iterate over delete queue
                  lapply(group_server$delete_data, function(name) {
                    # Delete from database
                    delete.data(r$db, group_server$data_objects[[name]])
                    # Delete from groupdata
                    group_server$data_objects[name] <- NULL
                  })
                  # Clear queue
                  group_server$delete_data <- c()
                }
              )

              ##########
              ### UI ###
              ##########

              ## Array for data boxes
              output$ui_boxArray = shiny::renderUI(
                lapply(group_server$data_objects, function(o) {
                  o@name <- ns(o@name) # Important
                  boxUI(o)()}
                )
              )

              ## File Input
              output$ui_fileinput <- shiny::renderUI(
                shiny::fileInput(
                  # File input parameters
                  inputId = ns(paste0(RANDOMADDRESS, "_fileinput")),
                  label = r$txt[45],
                  multiple = TRUE,
                  width = "100%"
                )
              )

              ## UI method selection for reading the data
              output$ui_readmethod <- shiny::renderUI(
                # Select input for read method
                shiny::selectInput(
                  # Selection parameters
                  inputId = ns(paste0(RANDOMADDRESS, "_readmethod")),
                  label = r$txt[49],
                  choices = new(group_server$obj@dtype)@readmethods,
                  selected = get_gparam()[["readmethod"]]
                )
              )

              ## Delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(
                  # Action button parameters
                  inputId = ns(paste0(RANDOMADDRESS, "_deletebutton")),
                  label = r$txt[33]
                )
              )

              output$ui_color_picker <- shiny::renderUI({
                if (!(group_server$obj@name %in% import_server$predefined_groups)) {
                  colourpicker::colourInput(
                    ns(paste0(RANDOMADDRESS, "_colorpicker")),
                    r$txt[50],
                    group_server$color,
                    palette = "limited",
                    allowedCols = grDevices::colors())
                }
              })

              ## UI group parameter elements
              output$ui_group_options = shiny::renderUI({

                go = group_options()
                gparam = get_gparam()

                print("Render ui")
                print(gparam)
                print("--- go ----------")
                print(go)

                render_group_options_ui = function(le, le_name) {

                  sel = ""

                  if (!is.null(get_gparam()[[le_name]])) {
                    sel = get_gparam()[[le_name]]
                  }

                  if (le == "col_select") {
                    ui = shiny::selectInput(
                      inputId = ns(paste0(RANDOMADDRESS, "-", le_name)),
                      label = le_name,
                      choices = group_server$columnnames,
                      selected = sel
                    )
                  } else if (le == "text_input") {
                    ui = shiny::textInput(
                      inputId = ns(paste0(RANDOMADDRESS, "-", le_name)),
                      label = le_name,
                      value = sel
                    )
                  }
                  return(ui)
                }

                ui = purrr::map2(go, names(go), render_group_options_ui)

                return(ui)

              })

              ## UI read parameter elements
              output$ui_read_parameter = shiny::renderUI({

                rp = read_parameter()

                render_read_parameter_ui = function(le, le_name) {
                  input_id = ns(paste(RANDOMADDRESS, le_name, sep = "-"))

                  if (!is.null(get_gparam()[[le_name]])) {
                    le = get_gparam()[[le_name]]
                  }
                  if (le_name %in% names(group_server$obj@gparam)) {
                    le = group_server$obj@gparam[[le_name]]
                  }
                  # Depending on inputtype render checkbox or text input
                  if (is(le, "logical") & !is.na(le)) {
                    shiny::checkboxInput(inputId = input_id, label = le_name, value = le)
                  }
                  else if (is(le, "character") | is.na(le)) {
                    shiny::textInput(inputId = input_id, label = le_name, value = toString(le))
                  } else if (is(le, "numeric")) {
                    shiny::numericInput(inputId = input_id, label = le_name, value = le)
                  }
                  else return()
                }
                # Create ui elements
                p = purrr::map2(rp, names(rp), render_read_parameter_ui)
                # Arrange created ui elements in columns
                s = split(x = p, f = ceiling(seq_along(p)/ceiling(length(p)/3)))
                # Create HTML html elements
                ui = shiny::fluidRow(col_4(s[1]), col_4(s[2]), col_4(s[3]))
                # Deliver ui elements
                return(ui)
              })

              #################################################################

              ## Group options box
              output$ui_groupoptions <- shiny::renderUI(
                # Box with options
                shinydashboard::box(
                  # Box parameters
                  id = ns(paste0(RANDOMADDRESS, "_groupoptions")),
                  title = r$txt[46],
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,

                  # Box content
                  shiny::uiOutput(ns("ui_color_picker")),
                  shiny::uiOutput(ns("ui_group_options"))
                )
              )

              ## Upload options box
              output$ui_readoptions <- shiny::renderUI(
                # Box with options
                shinydashboard::box(
                  # Box parameters
                  id = ns(paste0(RANDOMADDRESS, "_uploadoptions")),
                  title = r$txt[48],
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,

                  # Box content
                  shiny::uiOutput(ns("ui_read_parameter"))
                )
              )

              ##################################################################

              ## Input Box
              output$ui_inputBox <- shiny::renderUI(
                shinydashboard::box(
                  # Box parameters
                  id = ns("box2"),
                  title = r$txt[47],
                  width = 12,

                  # Box content
                  shiny::fluidRow(
                    col_6(
                      #style = "margin-top: 25px;",
                      shiny::uiOutput(ns("ui_fileinput"))
                    ),
                    col_6(
                      shiny::uiOutput(ns("ui_readmethod"))
                    )
                  ),
                  shiny::fluidRow(
                    col_6(
                      shiny::uiOutput(ns("ui_groupoptions"))
                    ),
                    col_6(
                      shiny::uiOutput(ns("ui_readoptions"))
                    )
                  ),
                  shiny::fluidRow(
                    col_6(
                      shiny::uiOutput(ns("ui_delete_button"))
                    )
                  )
                )
              )

              # Main box
              output$ui_groupbox <- shiny::renderUI(
                shiny::tagList(
                  # Some css stuff regarding box color
                  tags$style(HTML(
                    boxcolor(
                      boxid = ns("ui_groupbox"),
                      col = gplots::col2hex(group_server$color)
                    )
                  )),

                  shinydashboard::box(
                    # Box parameter
                    id = ns("box"),
                    title = obj@name,
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = FALSE,

                    # Box content
                    shiny::fluidRow(
                      # Box array
                      shiny::uiOutput(ns("ui_boxArray")),
                      # Input box
                      shiny::uiOutput(ns("ui_inputBox"))
                    )
                  )
                )
              )

              ####

            })
            return(server)
          })
