#' classTimeseries
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom utils read.csv head
#' @importFrom methods signature slot
#' @importFrom shiny tagList fluidRow uiOutput moduleServer renderUI textInput
#' @importFrom shiny actionButton observeEvent
#' @importFrom shinydashboard box
#' @importFrom purrr map
#'
#' @noRd

# Class definition

setClass("Timeseries",
         contains = "Data",
         slots = c(
           dec = "character",
           sep = "character",
           header = "logical",
           quote = "character",
           na.strings = "character",
           nrows = "numeric",
           skip = "numeric",
           comment.char = "character",
           head = "data.frame"
         ),
         prototype = list(
           dec = ".",
           sep = ",",
           header = FALSE,
           quote = "\"'",
           na.strings = "NA",
           nrows = -1,
           skip = 0,
           comment.char = "#",
           head = data.frame()
         )
)

# Methods

## Read timeseries from OS

setMethod("read.data",
          methods::signature(obj = "Timeseries"),
          function (obj) {
            if (obj@fileext == "csv") {
              df = utils::read.csv(obj@filepath)
              obj@nrows <- nrow(df)
              return(df)
            }

          })

## Generate a box UI

setMethod("boxUI",
          methods::signature(obj = "Timeseries"),
          function (obj) {
            ui = function(id = obj@name) {
              ns <- NS(id)
              shiny::tagList(

                ####

                # The displayed box
                shinydashboard::box(
                  id = "box", title = obj@displayname, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  # DeleteButton
                  shiny::fluidRow(
                    col_4(
                      shiny::tableOutput(ns("ui_table_head"))
                    ),
                    col_4(
                      shiny::uiOutput(ns("ui_option_box")),
                    ),
                    col_4(
                      shiny::uiOutput(ns("ui_delete_button"))
                    )
                  )
                )

                ####

              )
            }
            return(ui)
          })

## Generate box Servers

setMethod("boxServer",
          methods::signature(obj = "Timeseries"),
          definition = function(obj, r, group_data, txt) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              # Generate a random name for button.
              randomaddress = paste0(sample(letters, 1), sample(c(letters, 0:9), 9), collapse = "")

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(randomaddress, "_delete_button")), label = txt[32], width = "100%")
              )

              # Observe Delete button
              shiny::observeEvent(input[[paste0(randomaddress, "_delete_button")]], {
                # Add data to delete queue
                group_data$delete <- append(group_data$delete, obj@name)
              })

              # UI Elements

              ## Table head
              output$ui_table_head <- shiny::renderTable(
                utils::head(get.table(r$db, obj@name))
              )

              ## Option box
              output$ui_option_box <- shiny::renderUI(
                shinydashboard::box(id = ns(paste0(randomaddress, "_option_box")),
                                    collapsible = TRUE, collapsed = TRUE,
                                    width = 12, title = txt[36],
                                    col_6(
                                      shiny::textInput(ns(paste0(randomaddress, "_opt1")), label = txt[37]),
                                      shiny::textInput(ns(paste0(randomaddress, "_opt2")), label = txt[38]),
                                      shiny::textInput(ns(paste0(randomaddress, "_opt3")), label = txt[39]),
                                      shiny::textInput(ns(paste0(randomaddress, "_opt4")), label = txt[40])
                                    ),
                                    col_6(
                                      shiny::textInput(ns(paste0(randomaddress, "_opt5")), label = txt[41]),
                                      shiny::textInput(ns(paste0(randomaddress, "_opt6")), label = txt[42]),
                                      shiny::textInput(ns(paste0(randomaddress, "_opt7")), label = txt[43]),
                                      shiny::textInput(ns(paste0(randomaddress, "_opt8")), label = txt[44])
                                    )
                )
              )

              ####

            })
            return(server)
          })

setGeneric("attribute.df", function(obj) standardGeneric("attribute.df"))

setMethod("attribute.df",
          methods::signature(obj = "Timeseries"),
          function (obj) {
            attributes = methods::slotNames(obj)
            a = purrr::map(attributes, function(slotname) slot(obj, slotname))
            names(a) = attributes
            return(data.frame(a)[, c("key", setdiff(attributes, "key"))])
          })
