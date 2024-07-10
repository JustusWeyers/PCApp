#' classMetadata
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Class definition
source("R/fct_classTableData.R")

setClass("Metadata",
         contains = "TableData",
         slots = c(
           cc = "character",
           rparam = "list",
           dparam = "list"
         ),
         prototype = list(
           cc = c(
             ID = NA_character_,
             NAME = NA_character_,
             LAT = NA_character_,
             LON = NA_character_,
             missingVal = NA_character_
           ),
           dparam = list(),
           rparam = list()
         )
)


# Methods

## Generate box UI

setMethod("boxUI",
          methods::signature(obj = "Metadata"),
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
                    col_12(DT::DTOutput(ns("ui_table"))),
                    col_6(),
                    col_6(
                      col_6(),
                      col_6(
                        shiny::uiOutput(ns("ui_delete_button"))
                      )
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
          methods::signature(obj = "Metadata"),
          definition = function(obj, r, group_server) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              # Constants

              ## Generate a random name for button.
              randomaddress = random_address()

              # Reactive Values

              dataserver = reactiveValues(
                obj = obj
              )

              # Reactive functions

              # Can-load test
              canload = reactive({
                print(paste0("Can load (", obj@filepath, "): ", file.exists(obj@filepath)))
                file.exists(obj@filepath)
              })

              ## Read in data
              data = reactive({
                get_data(r$db, dataserver$obj)
              })

              ## Create plot
              timeseries_plot = reactive({
                plot(x = 1:10, y = (1:10)**2)
              })

              # Server logic

              ## Observe reactive data object
              shiny::observeEvent(
                eventExpr = dataserver$obj,
                handlerExpr = {
                  # Write data to database.
                  dataserver$obj@key <- write.data(r$db, dataserver$obj, data())
                  # Update data object key in groupserver
                  groupserver$dataObjects[[dataserver$obj@name]]@key <- dataserver$obj@key
                }
              )

              ## Observe Delete button
              shiny::observeEvent(input[[paste0(randomaddress, "_delete_button")]], {
                # Add data to delete queue
                groupserver$delete <- append(groupserver$delete, dataserver$obj@name)
              })

              # UI Elements

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(randomaddress, "_delete_button")), label = txt[32], width = "100%")
              )

              ## Table


              output$ui_table <- DT::renderDT({
                DT::datatable(data(), options = list(scrollX = TRUE)) |>
                  DT::formatStyle(groupserver$id_col, backgroundColor = "forestgreen")
              })

              ## Preview
              output$ui_timeseries_plot <- shiny::renderPlot(timeseries_plot())

              ####

            })
            return(server)
          })

setMethod("get_data",
          methods::signature(obj = "Metadata"),
          definition = function(d, obj) {

            if (file.exists(obj@filepath)) {
              tryCatch({
                return(do.call(obj@readmethod, c(file = obj@filepath, obj@readparam)))
              }, error = function(cond) {
                return(data.frame())
              })
            } else {
              return(get.table(d, obj@name))
            }

          })

setMethod("get_cols",
          methods::signature(obj = "Metadata"),
          definition = function(d, obj) {
            colnames(get_data(d, obj))
          })
