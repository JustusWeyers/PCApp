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
#' @importFrom htmltools div hr
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
           readmethod = "character"
         ),
         prototype = list(
           key = NA_integer_,
           name = NA_character_,
           dtype = NA_character_,
           color = "black",
           data = list(),
           readmethod = NA_character_
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
                working_name = paste0(display_name, "_", dtype, "_", dgroup)
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

              # Reactive functions

              readmethod = reactive({
                input[[paste0(randomaddress, "_readmethod")]]
              })

              readparam = reactive({
                optional_fun_param(readmethod())
              })

              readparaminputs = reactive({
                l = lapply(names(readparam()), function(n) input[[paste(randomaddress, n, sep = "-")]])
                names(l) = names(readparam())
                return(l)
              })

              new_grouptable = reactive({
                df = S4_to_dataframe(new(obj@dtype))
                opt = as.data.frame(readparam())
                write.dbtable(r$db, obj@name, merge(df, opt)[0,])
              })

              ## DB group table
              group_table = shiny::reactive({
                get.table(r$db, tablename = obj@name)
              })

              ## Get data objects
              get_dataObjects = reactive({
                # (Re-) Create data objects from details table
                if (obj@name %in% user.tables(r$db)$tablename) {
                  gt = group_table()
                  objects = purrr::pmap(.l = gt, .f = recreateDataObjects)
                  # Give dataobjects in list names
                  names(objects) = gt$name
                  return(objects)
                } else return(NULL)
              })

              ## UI read parameter elements
              ui_parameters = reactive({
                renderReadparameter = function(le, le_name) {
                  input_id = ns(paste(randomaddress, le_name, sep = "-"))
                  # Depending on inputtype render checkbox or text input
                  if (class(le) == "logical" & !is.na(le)) {
                    shiny::checkboxInput(inputId = input_id, label = le_name, value = le)
                  }
                  else if (class(le) == "character" | class(le) == "numeric" | is.na(le)) {
                    shiny::textInput(inputId = input_id, label = le_name, value = toString(le))
                  }
                  else return()
                }
                # Create ui elements
                purrr::map2(readparam(), names(readparam()), renderReadparameter)
              })

              input_change_detection = reactive({
                # Rename inputs for convenience
                Lobs = readparaminputs()
                Lexp = groupserver$readparam
                # Update
                groupserver$readparam <- readparaminputs()
                return(setdifflist(Lobs, Lexp))
              })

              # Reactive values

              groupserver = shiny::reactiveValues(
                dataObjects = get_dataObjects(),
                readparam = readparaminputs(),
                delete = c()
              )

              # Server logic

              ## Observe read method
              shiny::observeEvent(
                eventExpr = input[[paste0(randomaddress, "_readmethod")]],
                handlerExpr = {
                  if (!(obj@name %in% user.tables(r$db)$tablename) | readmethod() != obj@readmethod  ) {
                    print("Create Grouptable")
                    new_grouptable()
                    groupserver$dataObjects <- NULL
                  }
                }
              )

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
                      dtype = obj@dtype, dgroup = obj@key,
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
                lapply(groupserver$dataObjects, function(o) boxServer(o, r = r, groupserver = groupserver, txt = txt))
              })

              ## Observe read parameter ui
              shiny::observeEvent(
                eventExpr = readparaminputs(),
                handlerExpr = {
                  change = input_change_detection()
                  if (length(change) == 1) {
                    print(change)
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

              ## Observe delete button
              shiny::observeEvent(
                eventExpr = input[[paste0(randomaddress, "_deletebutton")]],
                handlerExpr = {
                  print(paste("Delete", obj@name))
                  # Add data to delete queue
                  importserver$delete <- append(importserver$delete, obj@name)
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
                  choices = new(obj@dtype)@readmethods,
                  selected = obj@readmethod
                )
              )

              ## UI read parameter array
              output$ui_fun_params <- shiny::renderUI({
                p = ui_parameters()
                # Arrange created ui elements in columns
                s = split(x = p, f = ceiling(seq_along(p)/ceiling(length(p)/2)))
                # Create HTML html elements
                ui = shiny::fluidRow(col_6(s[1]), col_6(s[2]))
                # Deliver ui elements
                return(ui)
              })

              ## Group options box
              output$ui_groupoptions <- shiny::renderUI(
                shiny::fluidRow(
                  # Box with options
                  shinydashboard::box(
                    # Box parameters
                    id = ns(paste0(randomaddress, "_groupoptions")),
                    title = txt[48],
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,

                    # Box content
                    shiny::uiOutput(ns("ui_fun_params"))
                  )
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
                      shiny::uiOutput(ns("ui_fileinput")),
                    ),
                    col_6(
                      shiny::uiOutput(ns("ui_readmethod"))
                    )
                  ),
                  shiny::fluidRow(
                    col_6(
                      shiny::uiOutput(ns("ui_delete_button"))
                    ),
                    col_6(
                      shiny::uiOutput(ns("ui_groupoptions"))
                    )
                  )
                )
              )

              # Group box
              output$ui_groupbox <- shiny::renderUI(
                shinydashboard::box(
                  # Box parameter
                  id = ns("box"),
                  title = obj@name,
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,

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
