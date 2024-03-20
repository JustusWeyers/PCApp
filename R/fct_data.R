#' Classes and methods for handling the data
#'
#' @description Multiple class- and method-definitions.
#'
#' @return Different S4-Classes with specific fields and methods
#'
#' @importFrom shiny NS tagList uiOutput moduleServer renderUI actionButton
#' @importFrom shinydashboard box
#' @importFrom methods signature
#'
#' @noRd

# Data Classes

setClass("Data",
         slots = c(
           name = "character",
           path = "character",
           size = "character"
         ),
         prototype = list(
           name = NA_character_,
           size = NA_character_,
           path = NA_character_
         ))

## Timeseries

setClass("Timeseries",
         contains = "Data",
         slots = c(
           nrow = "character"
         ),
         prototype = list(
           nrow = NA_character_
         )
)

## Geospatial data

setClass("GeoSpatialData",
         contains = "Data",
         slots = c(
           EPSG = "character"
         ),
         prototype = list(
           EPSG = NA_character_
         )
)

### Vector data

setClass("VectorData",
         contains = "GeoSpatialData",
         slots = c(
           features = "character"
         ),
         prototype = list(
           features = NA_character_
         )
)

### Rasterdata

setClass("RasterData",
         contains = "GeoSpatialData",
         slot = c(
           extent = "character"
         ),
         prototype = list(
           extent = NA_character_
         )
)

# Generics and methodes

## Generate a box UI ...

setGeneric("boxUI", function(obj) standardGeneric("boxUI"))

### ... for Timeseries

setMethod("boxUI",
          methods::signature(obj = "Timeseries"),
          function (obj) {
            ui = function(id = obj@name) {
              ns <- NS(id)
              shiny::tagList(

                ####

                # The displayed box
                shinydashboard::box(
                  id = "box", title = obj@name, width = 12,
                  # DeleteButton
                  shiny::uiOutput(ns("ui_delete_button"))
                )

                ####

              )
            }
            return(ui)
          })

## Generate a box Servers ...

### ... for Timeseries

setGeneric("boxServer", function(obj, dataserver) standardGeneric("boxServer"))

setMethod("boxServer",
          methods::signature(obj = "Timeseries"),
          definition = function(obj, dataserver) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns("button"), label = "Click me")
              )

              # Observe Delete button
              observeEvent(input$button, {
                # Add data to delete queue
                dataserver$delete <- append(dataserver$delete, obj@name)
              })

              ####

            })
            return(server)
          })

### ... for metadata

# in the making
