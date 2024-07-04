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
           color = "character",
           data = "list",
           readmethod = "character",
           gparam = "list"
         ),
         prototype = list(
           key = NA_integer_,
           name = NA_character_,
           dtype = NA_character_,
           color = "black",
           data = list(),
           readmethod = NA_character_,
           gparam = list()
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

                # Some css stuff regarding box color
                tags$style(HTML(
                  boxcolor(
                    boxid = ns("ui_groupbox"),
                    col = gplots::col2hex(obj@color)
                  )
                )),

                # The group box ui
                uiOutput(ns("ui_groupbox"))

                ####

              )
            }
            return(ui)
          })

## Group server

setGeneric("groupServer", function(obj, r, importserver, txt) standardGeneric("groupServer"))

setMethod("groupServer",
          definition = function(obj, r, importserver, txt) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              # Groupservers reactive values

              groupserver = shiny::reactiveValues(
                obj = obj
              )

              # Constants

              ## Generate a random namespace address
              randomaddress = random_address()

              # Functions

              ## Import to database
              import = function(name, size, type, datapath, dtype, dgroup,
                                readmethod, readparam) {
                # Extract name for display
                display_name = tools::file_path_sans_ext(name)
                # Combine working name as pair of displayName and datatype
                working_name = paste0(stringr::str_replace_all(display_name, "[^[:alnum:]]", ""), "_", dtype, "_", dgroup)
                # File extension
                ext = tools::file_ext(name)
                # Create new upload data object
                new_data_object = methods::new(
                  Class = dtype, dtype = dtype, dgroup = dgroup, fileext = ext,
                  name = working_name, filename = name, filepath = datapath,
                  displayname = display_name, filetype = type, filesize = size,
                  readmethod = readmethod, readparam = readparam
                )
                return(new_data_object)
              }

              ## DB group table
              group_table = function() {
                get.table(r$db, tablename = groupserver$obj@name)
              }

              datagroup_table = function() {
                get.table(r$db, tablename = "datagroup_table")
              }

              ## Get data objects
              get_dataObjects = function() {
                # (Re-) Create data objects from details table
                if (groupserver$obj@name %in% user.tables(r$db)$tablename) {
                  objects = purrr::pmap(.l = group_table(), .f = recreateDataObjects)
                  # Give dataobjects in list names
                  names(objects) = group_table()$name
                  return(objects)
                } else return(NULL)
              }

              # Reactive functions

              all_colnames = reactive({
                # Fetch colnames of all objects
                unname(unlist(purrr::map(groupserver$dataObjects, function(x) get_cols(r$db, x))))
              })

              readmethod = reactive({
                input[[paste0(randomaddress, "_readmethod")]]
              })

              readparam = reactive({
                optional_fun_param(readmethod())
              })

              param = reactive({
                a = new(groupserver$obj@dtype)@param
              })

              array_inputs = reactive({
                input_elements = c(readparam(), param())
                l = lapply(names(input_elements), function(n) input[[paste(randomaddress, n, sep = "-")]])
                names(l) = names(input_elements)
                return(l)
              })

              new_grouptable = reactive({
                df = s4_to_dataframe(new(groupserver$obj@dtype))
                opt = as.data.frame(readparam())
                write.dbtable(r$db, groupserver$obj@name, merge(df, opt)[0,])
              })


              ## UI read parameter elements
              ui_parameters = reactive({
                renderReadparameter = function(le, le_name) {
                  input_id = ns(paste(randomaddress, le_name, sep = "-"))
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
                purrr::map2(readparam(), names(readparam()), renderReadparameter)
              })

              ui_param = reactive({
                renderColSelection = function(le, le_name) {
                  # Gather all possible choices
                  choices = purrr::map(groupserver$dataObjects, function(o) {
                    slot(o, "param")[[le_name]]
                  })
                  choices = unname(unlist(choices))
                  if (all(is.na(choices))) {
                    ui = shiny::textInput(
                      inputId = ns(paste0(randomaddress, "-", le_name)),
                      label = le_name,
                      value = ""
                    )
                  # Render select input for not NA
                  } else {
                    ui = shiny::selectInput(
                      inputId = ns(paste0(randomaddress, "-", le_name)),
                      label = le_name,
                      choices = c(unname(unlist(choices)), " "),
                      selected = " "
                    )
                  }
                  return(ui)
                }
                return(purrr::map2(param(), names(param()), renderColSelection))
              })

              input_change_detection = reactive({
                # Rename inputs for convenience
                Lobs = array_inputs()
                Lexp = groupserver$array_inputs
                # Update
                groupserver$array_inputs <- array_inputs()
                return(setdifflist(Lobs, Lexp))
              })

              ##################################################################

              # Server logic

              groupserver$dataObjects = get_dataObjects()
              groupserver$readparam = array_inputs()
              groupserver$delete = c()
              groupserver$param = obj@gparam

              ## Observe read method
              shiny::observeEvent(
                eventExpr = input[[paste0(randomaddress, "_readmethod")]],
                handlerExpr = {
                  if (!(groupserver$obj@name %in% user.tables(r$db)$tablename) | groupserver$obj@readmethod != readmethod()  ) {
                    print("Create Grouptable")
                    groupserver$obj@readmethod = readmethod()
                    new_grouptable()
                    groupserver$dataObjects <- NULL
                  }
                }
              )

              ## Observe changes in groupobject
              shiny::observeEvent(groupserver$obj, {
                lapply(c("name", "dtype", "color", "readmethod"), function(x) {
                  update.table(r$db, "datagroup_table", x, slot(groupserver$obj, x), groupserver$obj@key)
                })
              })

              shiny::observeEvent(groupserver$param, {
                print("# Set gparam")
                update.table(r$db, "datagroup_table", "gparam", toString(jsonlite::toJSON(groupserver$param)),  groupserver$obj@key)
              })

              ## Observe file input
              shiny::observeEvent(
                eventExpr = input[[paste0(randomaddress, "_fileinput")]],
                handlerExpr = {
                  # Fetch upload
                  upload = input[[paste0(randomaddress, "_fileinput")]]
                  # Check if upload is not null
                  if (any(!is.null(upload))) {
                    # Create new data objects via import function
                    new_data_objects = purrr::pmap(
                      # The dataframe to apply function
                      upload,
                      # Import function
                      import,
                      # Import parameters
                      dtype = groupserver$obj@dtype, dgroup = groupserver$obj@key,
                      readmethod = readmethod(), readparam = readparam())
                    # Set names of new data objects
                    names(new_data_objects) = purrr::map_vec(new_data_objects, function(do) do@name)
                    # Add data objects to group data
                    groupserver$dataObjects[names(new_data_objects)] <- new_data_objects
                  }
                }
              )

              # Update data boxes
              observeEvent(groupserver$dataObjects, {
                # groupserver$dataObjects <- get_dataObjects()
                lapply(groupserver$dataObjects, function(o) boxServer(o, r = r, groupserver = groupserver, txt = txt))
              })

              ## Observe conditional inputs
              shiny::observeEvent(
                eventExpr = array_inputs(),
                handlerExpr = {
                  # Detect the change
                  change = input_change_detection()
                  if (length(change) == 1) {
                    # Pass change to database
                    gt = group_table()
                    if (names(change) %in% colnames(gt)) {
                      print("if taken")
                      # gt[,names(change)] <- rep(change, nrow(gt))
                      # write.dbtable(r$db, groupserver$obj@name, gt)
                      # # Update data objects in group
                      # groupserver$dataObjects = get_dataObjects()
                    }
                    groupserver$param[names(change)] <- change
                  }
                }
              )

              ## Delete data objects
              shiny::observeEvent(
                eventExpr = groupserver$delete,
                handlerExpr = {
                  print("Delete Data")
                  # Iterate over delete queue
                  lapply(groupserver$delete, function(name) {
                    # Delete from database
                    delete.data(r$db, groupserver$dataObjects[[name]])
                    # Delete from groupdata
                    groupserver$dataObjects[name] <- NULL
                  })
                  # Clear queue
                  groupserver$delete <- c()
                }
              )

              ## Observe color change
              shiny::observeEvent(
                eventExpr = input[[paste0(randomaddress,"_colorpicker")]],
                handlerExpr = {
                  groupserver$obj@color <- input[[paste0(randomaddress,"_colorpicker")]]
                  importserver$groupObjects[[obj@name]] <- groupserver$obj
                }
              )

              ## Observe delete button
              shiny::observeEvent(
                eventExpr = input[[paste0(randomaddress, "_deletebutton")]],
                handlerExpr = {
                  print(paste("Delete", groupserver$obj@name))
                  # Add data to delete queue
                  importserver$delete <- append(importserver$delete, groupserver$obj@name)
                }
              )

              ##################################################################

              # UI elements

              ## Array for data boxes
              output$ui_boxArray = shiny::renderUI(
                lapply(groupserver$dataObjects, function(o) {
                  o@name <- ns(o@name) # Important
                  boxUI(o)()}
                )
              )

              ## File Input
              output$ui_fileinput <- shiny::renderUI(
                shiny::fileInput(
                  # File input parameters
                  inputId = ns(paste0(randomaddress, "_fileinput")),
                  label = txt[45],
                  multiple = TRUE,
                  width = "100%"
                )
              )

              ## Delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(
                  # Action button parameters
                  inputId = ns(paste0(randomaddress, "_deletebutton")),
                  label = txt[33]
                )
              )

              ## UI method selection for reading the data
              output$ui_readmethod <- shiny::renderUI(
                # Select input for read method
                shiny::selectInput(
                  # Selection parameters
                  inputId = ns(paste0(randomaddress, "_readmethod")),
                  label = txt[49],
                  choices = new(groupserver$obj@dtype)@readmethods,
                  selected = groupserver$obj@readmethod
                )
              )

              output$ui_color_picker <- shiny::renderUI({
                if (!(obj@name %in% importserver$predefined_groups)) {
                    colourpicker::colourInput(
                      ns(paste0(randomaddress, "_colorpicker")),
                      txt[50],
                      groupserver$obj@color,
                      palette = "limited",
                      allowedCols = grDevices::colors())
                }
              })


              output$ui_group_param <- shiny::renderUI({
                ui_param()
              })

              observeEvent(input[[paste0(randomaddress, "_id_picker")]], {
                groupserver$id_col = input[[paste0(randomaddress, "_id_picker")]]
              })

              ## UI read parameter array
              output$ui_fun_params <- shiny::renderUI({
                p = ui_parameters()
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
                  id = ns(paste0(randomaddress, "_groupoptions")),
                  title = txt[46],
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,

                  # Box content
                  shiny::uiOutput(ns("ui_color_picker")),
                  shiny::uiOutput(ns("ui_group_param"))
                )
              )

              ## Upload options box
              output$ui_uploadoptions <- shiny::renderUI(
                # Box with options
                shinydashboard::box(
                  # Box parameters
                  id = ns(paste0(randomaddress, "_uploadoptions")),
                  title = txt[48],
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,

                  # Box content
                  shiny::uiOutput(ns("ui_fun_params"))
                )
              )

              ## Input box
              output$ui_inputBox <- shiny::renderUI(
                shinydashboard::box(
                  # Box parameters
                  id = ns("box2"),
                  title = txt[47],
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
                      shiny::uiOutput(ns("ui_uploadoptions"))
                    )
                  ),
                  shiny::fluidRow(
                    col_6(
                      shiny::uiOutput(ns("ui_delete_button"))
                    )
                  )
                )
              )

              # Group box
              output$ui_groupbox <- shiny::renderUI(
                shinydashboard::box(
                  # Box parameter
                  id = ns("box"),
                  title = groupserver$obj@name,
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

              ####

            })
            return(server)
          })


# if (!is.na(le) & any(startsWith(tolower(cn), tolower(le)))) {
#   sel = cn[startsWith(tolower(cn), tolower(le))][1]
# } else {
#   sel = NULL
# }
# sel = NULL
