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
             color = "#BEBEBE"
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

              get_gparam = function() {
                print("--- Get gparam ----")
                dgt = get.table(shiny::isolate(r$db), "datagroup_table")
                gparam = as.list(jsonlite::fromJSON(dgt[dgt$key == obj@key, "gparam"]))
                print(gparam)
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

              get_data_objects = function() {
                print("Get data objects")
                # Fetch primarytable (pt)
                pt = get.table(shiny::isolate(r$db), "primary_table")
                pt = pt[pt$dgroup == obj@key,]

                # Build new data objects (do) from pt
                do = lapply(pt$key, function(key) {
                  return(methods::new(
                    Class = obj@dtype,
                    key = key,
                    name = pt[pt$key == key, "name"],
                    dtype = pt[pt$key == key, "dtype"],
                    dgroup = pt[pt$key == key, "dgroup"],
                    rparam = as.list(jsonlite::fromJSON(pt[pt$key == key, "rparam"])),
                    dparam = as.list(jsonlite::fromJSON(pt[pt$key == key, "dparam"]))
                  ))
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
                working_name = paste0("id", rlang::hash(paste0(n, "_", dtype, "_", dgroup)))
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
                l = lapply(names(input_elements), function(n) {
                  input[[paste(RANDOMADDRESS, n, sep = "-")]]
                })
                l = stats::setNames(l, names(input_elements))
                return(l)
              })

              group_option_inputs = reactive({
                input_elements = group_options()
                l = lapply(names(input_elements), function(n) {
                  input[[paste(RANDOMADDRESS, n, sep = "-")]]
                })
                l = stats::setNames(l, names(input_elements))
                return(l)
              })

              apply_button_groupoptions = reactive({
                input[[paste0(RANDOMADDRESS, "_applybutton")]]
              })

              apply_button_readoptions = reactive({
                input[[paste0(RANDOMADDRESS, "_applybutton_read")]]
              })

              detail_buttons = reactive(
                lapply(group_server$data_objects, function(o) {
                  input[[paste0(RANDOMADDRESS, o@name, "_details")]]
                })
              )

              ########################
              ### Server Functions ###
              ########################

              # 01. Set up reactive values
              group_server = reactiveValues(
                obj = obj,
                new_data_objects = NULL, # get_data_objects(),
                delete_data = NULL,
                color = get_gparam()[["color"]],
                group_options = NULL,
                read_options = NULL,
                fields = c(),
                detail_buttons = NULL,
                delete_buttons = NULL,
                running_boxservers = c(),
              )

              # 02. Observe file input
              shiny::observeEvent(
                eventExpr = fileinput(),
                handlerExpr = {
                  # Fetch upload
                  upload = fileinput()
                  upload$datapath <- gsub("\\\\", "/", upload$datapath)
                  # Check if upload is not null
                  if (any(!is.null(upload))) {
                    drop = c(".cpg", ".dbf", ".geojson", ".prj", ".qmd", ".shx" )
                    upload = upload[sapply(upload$datapath, function(dp) !any(endsWith(dp, drop))),]
                    # Create new data objects via import function
                    new_data_objects = purrr::pmap(
                      upload, new_do, dtype = group_server$obj@dtype,
                      dgroup = group_server$obj@key
                    )
                    # Get the names of the newly created data objects
                    nms = purrr::map_vec(new_data_objects, function(do) {
                      do@name
                    })
                    # Add data objects to group data
                    group_server$new_data_objects[nms] <- new_data_objects
                  }
              })

              # 03. Observe/handle new data objects
              shiny::observeEvent(
                eventExpr = group_server$new_data_objects,
                handlerExpr = {

                  if(length(group_server$new_data_objects) != 0) {
                    shiny::withProgress(message = "Loading", value = 0, {

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
                        pt = get.table(r$db, "primary_table")
                        if (do@name %in% pt$name) {
                          key = pt[pt$name == do@name, "key"]
                          replace.by.primary_key(r$db, "primary_table", key, pt_entry)
                        } else if (is.na(do@key)) {
                          DBI::dbAppendTable(r$db@con, "primary_table", pt_entry)
                        } else {
                          replace.by.primary_key(r$db, "primary_table", do@key, pt_entry)
                        }

                        shiny::incProgress(
                          1/length(group_server$new_data_objects),
                          detail = do@name)

                      })

                    })

                    # Fetch dataobjects
                    group_server$new_data_objects <- get_data_objects()

                    # Launch data
                    lapply(group_server$new_data_objects, function(o) {
                      # 1. Eventually upload data
                      if ("filepath" %in% names(o@dparam)){

                        initial_read_write(o, r$db)

                        o@dparam["filepath"] <- NULL

                        change.tablevalue(
                          r$db,
                          "primary_table",
                          o@key, "dparam",
                          toString(jsonlite::toJSON(o@dparam))
                        )
                      }
                    })

                    # Some post processing
                    nms <- names(group_server$new_data_objects)
                    group_server$data_objects[nms] <- group_server$new_data_objects
                    group_server$new_data_objects <- NULL
                  }
              })

              # 04. Observe colorpicker
              observeEvent(color_picker(), {
                if (!is.null(is.null(color_picker())) & color_picker() != group_server$color) {
                  gparam = get_gparam()
                  gparam[["color"]] <- color_picker()
                  set_gparam(gparam)
                  group_server$color <- get_gparam()[["color"]]
                  r$import_trigger <- !(r$import_trigger)
                }
              })

              # 05. Observe read method input
              observeEvent(readmethod(), {
                gparam = get_gparam()
                gparam[["readmethod"]] = readmethod()
                opt <- optional_fun_param(readmethod())
                gparam[names(opt)] <- opt
                set_gparam(gparam)
              })

              # 06. Observe read parameter inputs
              observeEvent(read_parameter_inputs(), {
                if (!is.null(read_parameter_inputs())) {
                  group_server$read_options <- read_parameter_inputs()
                  group_server$fields <- NULL
                  rp = group_server$read_options
                  rp = rp[!sapply(rp,is.null)]
                  gparam = get_gparam()
                  gparam[names(rp)] <- rp
                  set_gparam(gparam)
                }
              })

              # 07. Observe group parameter inputs
              observeEvent(group_option_inputs(), {
                if (!is.null(group_option_inputs())) {
                  group_server$group_options <- group_option_inputs()
                  gparam = get_gparam()
                  gparam[names(group_server$group_options)] <- group_server$group_options
                  set_gparam(gparam)
                }
              })

              # 08. Observe read options
              observeEvent(apply_button_readoptions(), {
                lapply(group_server$data_objects, function(o) {

                  print("Data wrangling")

                  fields = data_wrangling(dataobject = o, db = r$db, options = get_gparam())

                  if (!is.null(fields)) {
                    group_server$fields <- unique(c(group_server$fields, fields))
                  }

                  r$import_trigger <- !(r$import_trigger)

                })
              })

              # 09. Observe group options
              observeEvent(apply_button_groupoptions(), {
                pt = get.table(shiny::isolate(r$db), "primary_table")
                lapply(group_server$data_objects, function(o) {
                  print("Data cleaning")
                  indata = get.table(r$db, paste0(o@name, "_readin"))
                  cldata = clean_data(dataobject = o, db = r$db, options = group_option_inputs())
                  write.dbtable(r$db, paste0(o@name, "_clean"), cldata)
                })
                # Alternatively to many small joins one big join
                if (identical(obj@dtype, "Timeseries")) {
                  merge.timeseries(r$db, names(group_server$data_objects))
                }

                if (any("Metadata" %in% pt$dtype) & any("Timeseries" %in% pt$dtype)) {
                  r$metadata = join_timeseries_with_metadata(r$db)
                }

                r$import_trigger <- !(r$import_trigger)
              })

              ## 10. Observe delete button
              shiny::observeEvent(
                eventExpr = input[[paste0(RANDOMADDRESS, "_deletebutton")]],
                handlerExpr = {
                  print(paste("Delete", group_server$obj@name))
                  # Add data to delete queue
                  import_server$delete_groups <- append(import_server$delete, group_server$obj@name)
                }
              )

              ## 11. Delete data objects
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

              observeEvent(detail_buttons(), {
                diff = setdifflist(group_server$detail_buttons, detail_buttons())
                diff = names(diff)
                group_server$detail_buttons <- detail_buttons()

                if (length(diff) == 1 & any(!(diff %in% group_server$running_boxservers)))  {
                  boxServer(group_server$data_objects[[diff]], r = r, group_server = group_server)
                  group_server$running_boxservers <- c(group_server$running_boxservers, diff)
                  shiny::removeUI(selector = paste0("#", ns(paste0(RANDOMADDRESS, diff, "_details"))), session = session)
                }

              })

              ##########
              ### UI ###
              ##########

              # Array for data boxes
              output$ui_boxArray = shiny::renderUI({
                group_server$new_data_objects =  get_data_objects()

                lapply(group_server$data_objects, function(o) {
                  # Important namespace juggle
                  oname = o@name
                  o@name <- ns(o@name)
                  # Render empty box
                  shinydashboard::box(
                    title = oname,
                    width = 12, collapsible = TRUE, collapsed = TRUE,
                    # Box content
                    boxUI(o)(),
                    fluidRow(
                      col_2(
                        shiny::actionButton(
                          inputId = ns(paste0(RANDOMADDRESS, oname, "_details")),
                          label = "Show Details", class = "btn-xs")
                      )
                    )
                  )
                })
              })


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
                if (!(obj@name %in% import_server$predefined_groups)) {
                  shiny::actionButton(
                    # Action button parameters
                    inputId = ns(paste0(RANDOMADDRESS, "_deletebutton")),
                    label = r$txt[33]
                  )
                }
              )

              output$ui_apply_button <- shiny::renderUI(
                shiny::actionButton(
                  # Action button parameters
                  inputId = ns(paste0(RANDOMADDRESS, "_applybutton")),
                  label = r$txt[61]
                )
              )

              output$ui_apply_button_read <- shiny::renderUI(
                shiny::actionButton(
                  # Action button parameters
                  inputId = ns(paste0(RANDOMADDRESS, "_applybutton_read")),
                  label = r$txt[61]
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
                render_group_options_ui = function(le, le_name) {

                  sel = ""

                  if (!is.null(get_gparam()[[le_name]])) {
                    sel = get_gparam()[[le_name]]
                  }

                  if (le == "col_select") {
                    ui = shiny::selectInput(
                      inputId = ns(paste0(RANDOMADDRESS, "-", le_name)),
                      label = le_name,
                      choices = group_server$fields,
                      selected = sel
                    )
                  } else if (le == "text_input") {
                    ui = shiny::textInput(
                      inputId = ns(paste0(RANDOMADDRESS, "-", le_name)),
                      label = le_name,
                      value = sel
                    )
                  } else if (le == "checkbox_input") {
                    ui = shiny::checkboxGroupInput(
                      inputId = ns(paste0(RANDOMADDRESS, "-", le_name)),,
                      label = le_name,
                      choices = group_server$measurment_status,
                      selected = group_server$measurment_status,
                      inline = FALSE
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
                  shiny::uiOutput(ns("ui_group_options")),
                  col_6(shiny::uiOutput(ns("ui_apply_button")))
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
                  shiny::uiOutput(ns("ui_read_parameter")),
                  shiny::uiOutput(ns("ui_apply_button_read"))
                )
              )

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
                    col_4(
                      shiny::uiOutput(ns("ui_readoptions"))
                    ),
                    col_4(
                      shiny::uiOutput(ns("ui_groupoptions"))
                    ),
                    col_4(
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
                    collapsed = TRUE,

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
