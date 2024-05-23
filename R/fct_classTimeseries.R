#' classTimeseries
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom utils read.csv2 head
#' @importFrom methods signature slot
#' @importFrom shiny tagList fluidRow uiOutput moduleServer renderUI textInput
#' @importFrom shiny actionButton observeEvent
#' @importFrom shinydashboard box
#' @importFrom purrr map
#'
#' @noRd

# Class definition
source("R/fct_classTableData.R")

setClass("Timeseries",
         contains = "TableData",
         slots = c(
           dateformat = "character"
         ),
         prototype = list(
           dateformat = "%Y-%m-%d"
         )
)



# Methods

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
                      shiny::tableOutput(ns("ui_table_head")),
                      shiny::uiOutput(ns("ui_option_box"))
                    ),
                    col_8(
                      shiny::plotOutput(ns("ui_timeseries_plot")),
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
          methods::signature(obj = "Timeseries"),
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

              datagroup = reactive({
                get.dgroup(r$db, dataserver$obj)
              })

              ## Read in data
              data = reactive(
                tryCatch(
                  {
                    return(do.call(obj@readmethod, c(file = obj@filepath, obj@readparam)))
                  },
                  warning = function(cond) {
                    dataserver$filepath = NA
                    return(get.table(r$db, obj@name))
                  }
                )
              )

              ## Fetch head data
              headdata = reactive({})

              ## Create plot
              timeseries_plot = reactive({
                # data = data()[!is.na(as.numeric(data())), ]
                # return(plot(x = data[,1], y = data[,2], type = "l"))
                return(plot(x = 1:10, y = 1:10))
              })


              # Server logic

              ## Observe reactive data object
              shiny::observeEvent(
                eventExpr = dataserver$obj,
                handlerExpr = {
                  print("Timeseries Server")
                  # Write data to database. Always(!) when observe changes
                  write.data(r$db, dataserver$obj, data())
                }
              )

              ## Observe Delete button
              shiny::observeEvent(input[[paste0(randomaddress, "_delete_button")]], {
                # Add data to delete queue
                groupserver$delete <- append(groupserver$delete, obj@name)
              })

              # UI Elements

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(randomaddress, "_delete_button")), label = txt[32], width = "100%")
              )

              ## Table head
              output$ui_table_head <- shiny::renderTable(data(), width = "100%")

              ## Preview
              output$ui_timeseries_plot <- shiny::renderPlot(timeseries_plot())

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
