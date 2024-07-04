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
           timestamp = "character",
           value = "character",
           missing_val = "character",
           dateformat = "character",
           param = list()
         ),
         prototype = list(
           param = list(
             timestamp = NA_character_,
             value = NA_character_,
             valid = NA_character_,
             colnames = NA_character_,
             missing_val = NA_character_,
             dateformat = NA_character_
           )
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
          definition = function(obj, r, groupserver, txt) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns

              ####

              # Dataservers reactive values
              dataserver = reactiveValues(
                # Make reactive copy of obj itself
                obj = obj,
                # Store last used readparam
                oreadparam = obj@readparam
              )

              # Constants

              ## Generate a random name for button.
              randomaddress = random_address()

              # Functions

              ## Fetch groupserver param
              fetch_groupparam = function(pname) {
                if (pname %in% names(groupserver$param)) {
                  return(groupserver$param[[pname]])
                } else {
                  return(NA)
                }
              }

              # Reactive functions

              datagroup = reactive(
                get.dgroup(r$db, dataserver$obj)
              )

              # Can-load test
              canload = reactive({
                print(paste0("Can load (", obj@filepath, "): ", file.exists(obj@filepath)))
                file.exists(obj@filepath)
              })

              ## Read in data
              data = reactive(
                get_data(r$db, dataserver$obj)
              )

              ## Fetch head data
              headdata = reactive(
                get_head_data(r$db, dataserver$obj)
              )

              ## Perform some cleaning
              clean_data = reactive({
                # Fetch messy data
                data = data()
                # Fetch group parameter
                x_colname = fetch_groupparam("timestamp")
                y_colname = fetch_groupparam("value")
                missing_val = fetch_groupparam("missing_val")
                dateformat = fetch_groupparam("dateformat")

                # Build empty dataframe
                d = setNames(data.frame(matrix(ncol = 2, nrow = nrow(data))), c("timestamp", "value"))
                # Paste timestamp into d
                if (!is.na(x_colname) & x_colname %in% colnames(data)) {
                  d$timestamp = data[,x_colname]
                }
                # Paste values into d
                if (!is.na(y_colname) & y_colname %in% colnames(data)) {
                  d$value = data[,y_colname]
                }
                # Eventually remove missing data
                if (!is.na(missing_val)) {
                  d = d[as.character(d$value) != missing_val,]
                }
                # Eventually convert timestamp
                if (!is.na(dateformat)) {
                  d$timestamp <- as.Date(format(d$timestamp, scientific = FALSE), format = dateformat)
                }
                # Rename d columns
                colnames(d) <- c("timestamp", dataserver$obj@name)
                return(d)
              })

              ## Create plot
              timeseries_plot = reactive({
                cd = clean_data()
                colnames(cd) <- c("timestamp", "value")
                return(ggplot2::ggplot(cd, ggplot2::aes(x = timestamp, y = value)) + ggplot2::geom_point())
              })


              # Server logic

              ## Observe reactive data object
              shiny::observeEvent(
                eventExpr = dataserver$obj,
                handlerExpr = {
                  print("Timeseries Server")
                  if (!setequal(dataserver$obj@param, dataserver$oparam)) {
                    # print(paste("--- Update", dataserver$obj@name, "-------"))
                    #
                    # data = get_data(r$db, obj)
                    # dataserver$obj@head = get_head_data(r$db, obj)
                    # dataserver$obj@param[["timestamp"]] = colnames(data)
                    # dataserver$obj@param[["value"]] = colnames(data)
                    # dataserver$obj@param[["valid"]] = colnames(data)
                    # dataserver$obj@param[["colnames"]] = colnames(data)
                    #
                    # dataserver$obj@key = write.data(r$db, obj, data)
                    #
                    # # Pass updated object upwards
                    # groupserver$dataObjects[[dataserver$obj@name]] <- dataserver$obj
                    # # Renew oparam
                    # dataserver$oparam <- dataserver$obj@param
                  }
                }
              )

              ## Observe Delete button
              shiny::observeEvent(input[[paste0(randomaddress, "_delete_button")]], {
                # Add data to delete queue
                groupserver$delete <- append(groupserver$delete, obj@name)
              })

              ## Crasht irgendwie das Programm ...
              # shiny::observeEvent(clean_data(), {
              #   if (inherits(clean_data()$timestamp, "Date")) {
              #     cd = clean_data()
              #     if (colnames(cd)[2] %in% colnames(r$rawtable)) {
              #       r$rawtable = r$rawtable[ , -colnames(cd)[2]]
              #     }
              #     # Merge in new data
              #     r$rawtable = merge(r$rawtable, cd, by = "timestamp", all = TRUE)
              #   }
              # })

              # UI Elements

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(randomaddress, "_delete_button")), label = txt[32], width = "100%")
              )

              ## Table head
              output$ui_table_head <- shiny::renderTable(head(data()), width = "100%")

              ## Headdata
              output$ui_headdata <- shiny::renderText(headdata())

              ## Preview
              output$ui_timeseries_plot <- shiny::renderPlot(timeseries_plot())

              output$ui_databox = shiny::renderUI({
                shinydashboard::box(
                  id = "box", title = obj@displayname, width = 12,
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

setMethod("doupdate",
          methods::signature(obj = "Timeseries"),
          definition = function(obj, d) {
            return(obj)
          })

setMethod("get_data",
          methods::signature(obj = "Timeseries"),
          definition = function(d, obj) {
            print("get_data")
            if (file.exists(obj@filepath)) {
              tryCatch({
                dat = do.call(obj@readmethod, c(file = obj@filepath, obj@readparam, fill = TRUE))
                return(dat)
              }, error = function(cond) {
                print(cond)
                return(data.frame())
              })
            } else {
              dat = get.table(d, obj@name)
              return(dat)
            }
          })

setMethod("get_head_data",
          methods::signature(obj = "Timeseries"),
          definition = function(d, obj) {
            if (file.exists(obj@filepath)) {
              # Read all lines
              l = readLines(con = obj@filepath)
              # The headlines to come
              hlines = c()
              # Add headlines based on 'skip'
              if ("skip" %in% names(obj@readparam)) {
                skip = getElement(obj@readparam, "skip")
                hlines = c(hlines, 1:skip)
              }
              # Add headlines based on 'comment.char'
              if ("comment.char" %in% names(obj@readparam)) {
                c.char = getElement(obj@readparam, "comment.char")
                hlines = c(hlines, which(startsWith(l, c.char)))
              }
              # Clean up messy characters
              txt = stringi::stri_trans_general(l[unique(hlines)], "Latin-ASCII")
              # Create string
              txt = paste(paste0(txt, "\n"), collapse = '')
              # Return string containing head lines
              return(txt)
            } else {
              # Fetch datagroup table
              dgt = get.table(d, get.dgroup(d, obj))
              # Fetch head string from dgt
              txt = dgt[dgt$name == obj@name, "head"]
              # Return string containing head string
              return(txt)
            }
          })

setMethod("get_cols",
          methods::signature(obj = "Timeseries"),
          definition = function(d, obj) {
            colnames(get_data(d, obj))
          })

