#' classVectorData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom sf st_read
#'
#' @noRd

setClass("VectorData",
         contains = "GeoSpatialData",
         slots = c(
           cc = "list",
           dparam = "list",
           rparam = "list",
           readmethod = "character",
           readmethods = "character"
         ),
         prototype = list(
           cc = list(

           ),
           readmethod = c(
             "st_read"
           ),
           readmethods = c(
             "st_read"
           )
         )
)

# Methods

## Generate a box UI

setMethod("boxUI",
          methods::signature(obj = "VectorData"),
          function (obj) {
            ui = function(id = obj@name) {
              ns <- NS(id)
              shiny::tagList(

                ####

                # The displayed box
                shiny::fluidRow(
                  col_12(
                    shiny::plotOutput(ns("ui_map"))
                  )
                ),
                
                shiny::fluidRow(
                  col_2(
                    shiny::uiOutput(ns("ui_delete_button"))
                  )
                )

                ####

              )
            }
            return(ui)
          })

## Generate box Servers

setMethod("boxServer",
          methods::signature(obj = "VectorData"),
          definition = function(obj, r, group_server) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####
              
              # Constants
              
              RANDOMADDRESS = random_address()

              # Reactives
              
              shp = shiny::reactive(
                sf::st_read(r$db@con, obj@name)
              )

              map = shiny::reactive({
                ggplot2::ggplot(shp()) +
                  ggplot2::geom_sf() +
                  ggplot2::theme_minimal()
              })
              
              # Server
              
              ## Observe delete button
              shiny::observeEvent(input[[paste0(RANDOMADDRESS, "_delete_button")]], {
                # Add data to delete queue
                group_server$delete_data <- append(group_server$delete_data, obj@name)
              })
              
              # UI

              output$ui_map = shiny::renderPlot(
                map()
              )
              
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(RANDOMADDRESS, "_delete_button")), label = r$txt[32], width = "100%")
              )

              ####

            })
            return(server)
          })

setMethod("clean_data",
          methods::signature(dataobject = "VectorData"),
          function (dataobject, db, options) {

          }
)

setMethod("data_wrangling",
          methods::signature(dataobject = "VectorData"),
          function(dataobject, db, options) {
            indata = mydata(db, dataobject@name, options[["readmethod"]], options)
            # write.dbtable(db, paste0(dataobject@name, "_readin"), indata)
            # hdata = head_data(db, dataobject@name, options[["readmethod"]], options)
            # write.dbtable(db, paste0(dataobject@name, "_head"), hdata)

            return(colnames(indata))
          })

setMethod("initial_read_write",
          methods::signature(dataobject = "VectorData"),
          function(dataobject, db) {
            tryCatch({
              sf::st_write(obj = read_shapefile(dataobject@dparam[["filepath"]]), dsn = db@con, layer = dataobject@name)
            }, error = function(e) print(e))
          })
