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
         contains = "TableData"
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
                    shiny::uiOutput(ns("ui_table_head")),
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
          definition = function(obj, r, groupserver, txt) {
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
                if (canload()) {
                  tryCatch({
                    return(do.call(obj@readmethod, c(file = obj@filepath, obj@readparam)))
                  }, error = function(cond) {
                    return(data.frame())
                  })
                } else {
                  dataserver$filepath = NA
                  return(get.table(r$db, obj@name))
                }
              })

              # read.list(file = obj@filepath, skip=0, nlines=skip, order=NULL)

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

              ## Table head
              output$ui_table_head <- shiny::renderTable(head(data()), width = "100%")

              ## Preview
              output$ui_timeseries_plot <- shiny::renderPlot(timeseries_plot())

              ####

            })
            return(server)
          })
