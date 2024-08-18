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
           cc = "list",
           dparam = "list",
           readmethod = "character",
           readmethods = "character",
           rparam = "list",
           id = "character"
         ),
         prototype = list(
           cc = list(
             timestamp = "col_select",
             value = "col_select",
             missing_val = "text_input",
             dateformat = "text_input"
           ),
           dparam = list(),
           readmethod = c(
             "read.csv"
           ),
           readmethods = c(
             "read.csv",
             "read.csv2",
             "read.table"
           ),
           rparam = list()
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
                shiny::uiOutput(ns("ui_databox"))

                ####

              )
            }
            return(ui)
          })

## Generate box Servers

setMethod("boxServer",
          methods::signature(obj = "Timeseries"),
          definition = function(obj, r, group_server) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns


              ####

              ##################
              ### Connstants ###
              ##################

              RANDOMADDRESS = random_address()

              #################
              ### Functions ###
              #################

              get_gparam = function() {
                dgt = get.table(shiny::isolate(r$db), "datagroup_table")
                return(as.list(jsonlite::fromJSON(dgt[dgt$key == obj@dgroup, "gparam"])))
              }

              meta_infos = function() {
                infos = c("id", "clearname", "lat", "lon")
                l = lapply(infos, function (i) {
                  r$primary_table[r$primary_table$key == obj@key, i]
                })
                return(stats::setNames(l, infos))
              }

              #######################
              ### Reactive Values ###
              #######################

              timeseries_server = reactiveValues(
                obj = obj,
                dparam = obj@dparam,
                gparam = get_gparam(),
                data = NULL
              )

              ##########################
              ### Reactive functions ###
              ##########################

              gparam = reactive(
                timeseries_server$gparam
              )

              fetch_data = function(d, name) {
                if (paste0(name, "_clean") %in% user.tables(d)$tablename) {
                  t = get.table(d, paste0(name, "_clean"))
                  t$timestamp = as.Date(t$timestamp)
                  return(t)
                }
              }

              head_data = function (d, name) {
                if (paste0(name, "_head") %in% user.tables(d)$tablename) {
                  hlines = get.table(d, paste0(name, "_head"))$data
                  if (length(hlines)>30) {
                    hlines = c(head(hlines, n = 30), "...")
                  }
                  # Create string
                  txt = toString(paste0(hlines, "\n")) #, collapse = '')
                  # Return string containing head lines
                  return(txt)
                }
              }

              # data_colnames <- reactive(
              #   colnames(fetch_data(r$db, obj@name))
              # )

              # timestamp_column <- reactive(
              #   gparam()[["timestamp"]]
              # )

              # value_column <- reactive(
              #   gparam()[["value"]]
              # )

              # missing_val <- reactive(
              #   gparam()[["missing_val"]]
              # )

              # dateformat <- reactive(
              #   gparam()[["dateformat"]]
              # )

              plot <- reactive(
                if (!is.null(timeseries_server$data)) {
                  d = timeseries_server$data
                  colnames(d) = c("timestamp", "value")
                  ggplot2::ggplot(d, ggplot2::aes(x = timestamp, y = value)) +
                    ggplot2::geom_point() +
                    ggplot2::theme_minimal()
                }
              )

              title = reactive({
                tryCatch({
                  if (!is.na(meta_infos()[["id"]]) & !is.na(meta_infos()[["clearname"]])) {
                    return(paste0(meta_infos()[["clearname"]], " (", meta_infos()[["id"]], ")"))
                  } else {
                    return(obj@name)
                  }
                }, error = function(e)
                  return(obj@name)
                )
              })

              ########################
              ### Server functions ###
              ########################

              observeEvent(r$import_trigger, {
                timeseries_server$data <- fetch_data(r$db, obj@name)
                timeseries_server$headdata <- head_data(r$db, obj@name)
              })

              ## Observe delete button
              shiny::observeEvent(input[[paste0(RANDOMADDRESS, "_delete_button")]], {
                # Add data to delete queue
                group_server$delete_data <- append(group_server$delete_data, obj@name)
              })

              ##########
              ### UI ###
              ##########

              output$ui_table_head = shiny::renderTable({
                d = timeseries_server$data
                d$timestamp = as.character(d$timestamp)
                return(head(d))
              })

              output$ui_headdata = shiny::renderText({
                timeseries_server$headdata
              })

              output$ui_timeseries_plot <- shiny::renderPlot(
                plot()
              )

              output$ui_mapbox <- shiny::renderUI(
                shinydashboard::box(
                  id = "box", title = r$txt[60], width = 12,
                  collapsible = TRUE, collapsed = TRUE,

                  leaflet::leafletOutput(ns("ui_location_plot"))
                )
              )

              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(RANDOMADDRESS, "_delete_button")), label = r$txt[32], width = "100%")
              )

              output$ui_databox = shiny::renderUI({
                shiny::fluidRow(
                  col_4(
                    h3(r$txt[51]),
                    shiny::textOutput(ns("ui_headdata")),
                    h3(r$txt[52]),
                    shiny::tableOutput(ns("ui_table_head")),
                    shiny::uiOutput(ns("ui_option_box"))
                  ),
                  col_8(
                    shiny::plotOutput(ns("ui_timeseries_plot")),
                    shiny::uiOutput(ns("ui_mapbox")),
                    shiny::uiOutput(ns("ui_delete_button"))

                  )
                )
              })

              ####

            })
            return(server)
          })

setMethod("clean_data",
          methods::signature(dataobject = "Timeseries"),
          function (dataobject, db, options) {

            data = get.table(db, paste0(dataobject@name, "_readin"))

            # Central column names
            central_cnms = unlist(options)[unname(unlist(options)) %in% colnames(data)]

            df = data[,central_cnms]

            df = stats::setNames(df, names(central_cnms))


            if ("missing_val" %in% names(options)) {
              df = df[as.character(df$value) != options[["missing_val"]],]
            }

            if ("dateformat" %in% names(options)) {
              tryCatch(expr = {
                dates = format(df$timestamp, scientific = FALSE)
                df$timestamp <- as.Date(dates, format = options[["dateformat"]])
                df = df[sapply(df$timestamp, lubridate::is.Date),]
              }, error = function(e) {}
              )
            }

            colnames(df) <- c("timestamp", dataobject@name)

            return(df)

          }
)

setMethod("data_wrangling",
          methods::signature(dataobject = "Timeseries"),
          function(dataobject, db, options) {
            indata = mydata(db, dataobject@name, options[["readmethod"]], options)
            write.dbtable(db, paste0(dataobject@name, "_readin"), indata)
            hdata = head_data(db, dataobject@name, options[["readmethod"]], options)
            write.dbtable(db, paste0(dataobject@name, "_head"), hdata)

            return(colnames(indata))
          })
