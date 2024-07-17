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
             # valid = "col_text_select",
             # colnames = NA_character_,
             missing_val = "text_input",
             dateformat = "text_input"
           ),
           dparam = list(),
           readmethod = c(
             "read.table"
           ),
           readmethods = c(
             "read.table",
             "read.csv",
             "read.csv2"
           ),
           rparam = list(),
           id = NA_character_
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

              #######################
              ### Reactive Values ###
              #######################

              timeseries_server = reactiveValues(
                obj = obj,
                dparam = obj@dparam,
                gparam = get_gparam()
              )

              ##########################
              ### Reactive functions ###
              ##########################

              gparam = reactive(
                timeseries_server$gparam
              )

              file_path = reactive(
                timeseries_server$dparam[["filepath"]]
              )

              data = reactive({
                rm = gparam()[["readmethod"]]
                p = gparam()[names(gparam()) %in% names(optional_fun_param(rm))]
                p[["fill"]] <- TRUE
                p[["text"]] <- get.table(r$db, timeseries_server$obj@name)$data

                tryCatch(expr = {
                  return(do.call(rm, p))
                }, error = function(e) {
                  print(e)
                  return(NULL)
                })
              })

              data_colnames <- reactive(
                colnames(data())
              )

              timestamp_column <- reactive(
                gparam()[["timestamp"]]
              )

              value_column <- reactive(
                gparam()[["value"]]
              )

              missing_val <- reactive(
                gparam()[["missing_val"]]
              )

              dateformat <- reactive(
                gparam()[["dateformat"]]
              )

              plot <- reactive(
                ggplot2::ggplot(data = clean_data(), ggplot2::aes(x = timestamp, y = value)) +
                  ggplot2::geom_point()
              )

              head_data = reactive({
                l = get.table(r$db, timeseries_server$obj@name)$data
                # The headlines to come
                hlines = c()
                # Add headlines based on 'skip'
                if ("skip" %in% names(gparam())) {
                  skip = getElement(gparam(), "skip")
                  hlines = c(hlines, 1:skip)
                }
                # Add headlines based on 'comment.char'
                if ("comment.char" %in% names(gparam())) {
                  c.char = getElement(gparam(), "comment.char")
                  hlines = c(hlines, which(startsWith(l, c.char)))
                }
                # Filter for hlines
                hlines = l[unique(hlines)]
                # Clean up messy characters
                hlines = sapply(hlines, stringi::stri_trans_general, id = "Latin-ASCII")
                # Clean up messy characters
                return(hlines)
              })

              head_string = reactive(
                return(paste(paste0(head_data(), "\n"), collapse = ''))
              )

              clean_data = reactive({
                if (all(c(timestamp_column(), value_column()) %in% data_colnames())) {
                  df = data.frame(
                    timestamp = data()[,timestamp_column()],
                    value = data()[,value_column()]
                  )
                } else {
                  df = data.frame(timestamp = c(), value = c())
                }
                # Eventually remove missing data
                if (!is.null(missing_val())) {
                  df = df[as.character(df$value) != missing_val(),]
                }
                # Eventually convert timestamp
                if (!is.null(dateformat())) {
                  df$timestamp <- as.Date(format(df$timestamp, scientific = FALSE), format = dateformat())
                }
                return(df)
              })

              ########################
              ### Server functions ###
              ########################

              observeEvent(file_path(), {
                print("HEEEEREEEE")
                if (!is.null(file_path())){
                  print("# Upload")
                  write.dbtable(r$db, timeseries_server$obj@name, data.frame(data = stringi::stri_trans_general(readLines(file_path()), "Latin-ASCII")))
                  timeseries_server$dparam["filepath"] <- NULL
                }
              })

              observeEvent(head_string(), {
                st = stringr::str_trunc(head_string(), 9999)
                change.tablevalue(r$db, "primary_table", timeseries_server$obj@key, "head", st)
              })

              ## _. Observe changes in group parameters
              observeEvent(timeseries_server$dparam, {
                # Convert gparam list into json
                st = toString(jsonlite::toJSON(timeseries_server$dparam))
                # Encoding stuff
                st = gsub("'", "", st)
                # Write to database
                change.tablevalue(r$db, "primary_table", timeseries_server$obj@key, "dparam", st)
              })

              # Communication
              observeEvent(c(group_server$read_options, group_server$group_options), {
                print("TS observed group server read options")
                timeseries_server$gparam <- get_gparam()

                group_server$columnnames <- unique(c(group_server$columnnames, data_colnames()))
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
                head(data())
              })

              output$ui_headdata = shiny::renderText({
                hlines = head_data()
                if (length(hlines)>30) {
                  hlines = c(head(hlines, n = 30), "...")
                }
                # Create string
                txt = paste(paste0(hlines, "\n"), collapse = '')
                # Return string containing head lines
                return(txt)
              })

              output$ui_timeseries_plot <- shiny::renderPlot(
                plot()
              )

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(RANDOMADDRESS, "_delete_button")), label = r$txt[32], width = "100%")
              )

              output$ui_databox = shiny::renderUI({
                shinydashboard::box(
                  id = "box", title = timeseries_server$obj@name, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  # DeleteButton
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
                      col_6(),
                      col_6(
                        shiny::uiOutput(ns("ui_delete_button"))
                      )
                    )
                  )
                )
              })

              ####

            })
            return(server)
          })
