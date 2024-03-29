#' Classes and methods for handling the data
#'
#' @description Multiple class- and method-definitions.
#'
#' @return Different S4-Classes with specific fields and methods
#'
#' @importFrom shiny NS tagList uiOutput moduleServer renderUI actionButton
#' @importFrom shinydashboard box
#' @importFrom methods signature
#' @importFrom utils read.csv
#'
#' @noRd

# Data Classes

setClass("Data",
         slots = c(
           name = "character",
           filename = "character",
           filepath = "character",
           displayName = "character",
           dataType = "character",
           filetype = "character",
           size = "character"
         ),
         prototype = list(
           name = NA_character_,
           filename = NA_character_,
           filepath = NA_character_,
           displayName = NA_character_,
           dataType = NA_character_,
           filetype = NA_character_,
           size = NA_character_
         ))

## Timeseries

setClass("Timeseries",
         contains = "Data",
         slots = c(
           nrow = "numeric"
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

## Read data

setGeneric("read.data", function(obj) standardGeneric("read.data"))

setMethod("read.data",
          methods::signature(obj = "Timeseries"),
          function (obj) {
            if (obj@filetype == "csv") {
              df = read.csv(obj@filepath)
              obj@nrow <- nrow(df)
              return(df)
            }

          })

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

setGeneric("boxServer", function(obj, dataserver, txt) standardGeneric("boxServer"))

setMethod("boxServer",
          methods::signature(obj = "Timeseries"),
          definition = function(obj, dataserver, txt) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              # Generate a random name for button.
              randomName = paste(sample(letters, 10), collapse = "")

              getInputs <- function(pattern){
                reactives <- names(reactiveValuesToList(input))
                reactives[grep(pattern,reactives)]
              }

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(randomName), label = txt[32])
              )

              # Observe Delete button
              observeEvent(input[[randomName]], {
                print(paste("Button delete", obj@name, "pressed"))
                # Add data to delete queue
                dataserver$delete <- append(dataserver$delete, obj@name)
              })

              ####

            })
            return(server)
          })

### ... for metadata

# in the making
