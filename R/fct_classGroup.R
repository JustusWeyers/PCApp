#' classGroup
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom shiny tagList uiOutput fluidRow moduleServer reactiveValues
#' @importFrom shiny observeEvent renderUI fileInput actionButton
#' @importFrom shinydashboard box
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom methods new
#' @importFrom purrr pmap map_vec
#'
#' @noRd

# Class definition

setClass("Group",
         slots = c(
           key = "numeric",
           name = "character",
           dtype = "character",
           color = "character",
           data = "list"
         )
)

# Methods

## Group UI

setGeneric("groupUI", function(obj) standardGeneric("groupUI"))

setMethod(groupUI,
          definition = function (obj) {
            ui = function(id = obj@name) {
              ns <- NS(id)
              shiny::tagList(

                ####

                # The displayed box
                shinydashboard::box(
                  id = "box", title = strsplit(obj@name, "-")[[1]][2], width = 12,

                  # Box array
                  shiny::uiOutput(ns("ui_boxArray")),
                  shinydashboard::box(id = "box2", title = "", width = 12,
                    # File input
                    shiny::uiOutput(ns("ui_fileinput")),
                    # DeleteButton
                    shiny::uiOutput(ns("ui_delete_button"))
                  )
                )

                ####

              )
            }
            return(ui)
          })

## Group server

setGeneric("groupServer", function(obj, r, dataserver, txt) standardGeneric("groupServer"))

setMethod("groupServer",
          definition = function(obj, r, dataserver, txt) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              # Serverlogic

              ## Generate a random namespace address
              randomaddress = paste0(sample(letters, 1), sample(c(letters, 0:9), 9), collapse = "")

              ## Groups reactive values
              group_data = shiny::reactiveValues(
                dataObjects = obj@data,
                delete = c()
              )

              ## Fetch upload data Function
              import = function(name, size, type, datapath, dtype, dgroup) {
                # Extract name for display
                display_name = tools::file_path_sans_ext(name)
                # Combine working name as pair of displayName and datatype
                working_name = paste0(display_name, "_", dtype, "_", dgroup)
                # File extension
                ext = tools::file_ext(name)

                # Create new upload data object
                new_data_object = methods::new(
                  dtype,
                  dtype = dtype,
                  dgroup = dgroup,
                  name = working_name,
                  filename = name,
                  displayname = display_name,
                  filepath = datapath,
                  filetype = type,
                  filesize = size,
                  fileext = ext
                )

                # Write data to database
                write.data(r$db, new_data_object, read.data(new_data_object))

                # Return the newly created data object
                return(new_data_object)
              }

              ## Observe file input
              shiny::observeEvent(input[[paste0(randomaddress, "_fileinput")]], {
                # Fetch upload
                upload = input[[paste0(randomaddress, "_fileinput")]]
                # Check if upload is not null
                if (any(!is.null(upload))) {
                  # Create new data objects via import function
                  new_data_objects = purrr::pmap(upload, import, dtype = obj@dtype, dgroup = obj@key)
                  # Set names of new data objects
                  names(new_data_objects) = purrr::map_vec(new_data_objects, function(do) do@name)
                  # Add dataobjects to group data
                  group_data$dataObjects[names(new_data_objects)] <- new_data_objects
                }
                # Call box servers
                lapply(group_data$dataObjects, function(o) boxServer(o, r = r, group_data, txt = txt))
              })

              ## Delete single data objects
              shiny::observeEvent(group_data$delete, {
                print("Delete Data")
                # Iterate over delete queue
                lapply(group_data$delete, function(name) {
                  # Delete from database
                  delete.data(r$db, group_data$dataObjects[name])
                  # Delete from groupdata
                  group_data$dataObjects[name] <- NULL
                })
                # Clear queue
                group_data$delete <- c()
              })

              ## Observe delete button
              shiny::observeEvent(input[[paste0(randomaddress, "_deletebutton")]], {
                print(paste("Delete", obj@name))
                # Add data to delete queue
                dataserver$delete <- append(dataserver$delete, obj@name)
              })

              # UI elements

              ## Array for data boxes
              output$ui_boxArray = shiny::renderUI(lapply(group_data$dataObjects, function(o) {
                o@name <- ns(o@name) # Important
                boxUI(o)()
              }))

              ## File Input
              output$ui_fileinput <- shiny::renderUI(
                shiny::fileInput(ns(paste0(randomaddress, "_fileinput")), txt[45], multiple = TRUE, width = "100%")
              )

              ## Delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(randomaddress, "_deletebutton")), label = txt[33])
              )

              ####

            })
            return(server)
          })
