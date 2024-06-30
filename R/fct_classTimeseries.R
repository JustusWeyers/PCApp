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
           dateformat = "character",
           missing_val = "character",
           timestamp = "character",
           value = "character",
           valid = "character",
           colnames = "character"
         ),
         prototype = list(
           dateformat = "%Y-%m-%d",
           missing_val = NA_character_,
           timestamp = NA_character_,
           value = NA_character_,
           valid = NA_character_,
           colnames = NA_character_,
           param = list(
             timestamp = NA_character_,
             value = NA_character_,
             valid = NA_character_,
             colnames = NA_character_,
             missing_val = NA_character_
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

              # Constants

              ## Generate a random name for button.
              randomaddress = random_address()

              # Reactive Values

              dataserver = reactiveValues(
                obj = obj
              )

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

              # read.list(file = obj@filepath, skip=0, nlines=skip, order=NULL)

              ## Create plot
              timeseries_plot = reactive({
                d = data()

                # print(head(d))
                # print(paste("missing_val:", dataserver$obj@missing_val))
                # print(paste("value:", dataserver$obj@value))
                # if (all(!is.na(c(dataserver$obj@missing_val, dataserver$obj@value)))) {
                #   d = d[dataserver$obj@value != as.numeric(dataserver$obj@missing_val),]
                # }

                d = data.frame(Zeit = d[,1], Wert = d[,2])
                return(ggplot2::ggplot(d, ggplot2::aes(x = Zeit, y = Wert)) + ggplot2::geom_line())
                # plot(x = data()[data()[,2]>0,1], y = data()[data()[,2]>0,2])
              })

              # Server logic
              ## Observe reactive data object
              shiny::observeEvent(
                eventExpr = dataserver$obj,

                handlerExpr = {
                  print("Timeseries Server")
                  print("---------------------------")
                  print(dataserver$obj)
                  print("---------------------------")
                  if (!identical(doupdate(dataserver$obj, r$db), dataserver$obj)) {
                    print(paste("--- Update", dataserver$obj@name, "-------"))
                    # Write data to database. Always(!) when observe changes
                    dataserver$obj = doupdate(dataserver$obj, r$db)
                    # Pass updated object upwards
                    groupserver$dataObjects[[dataserver$obj@name]] <- dataserver$obj
                  }
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

            data = get_data(d, obj)
            obj@head = get_head_data(d, obj)

            obj@param[["timestamp"]] = colnames(data)
            obj@param[["value"]] = colnames(data)
            obj@param[["valid"]] = colnames(data)
            obj@param[["colnames"]] = colnames(data)

            obj@key = write.data(d, obj, data)

            return(obj)
          })

setMethod("get_data",
          methods::signature(obj = "Timeseries"),
          definition = function(d, obj) {
            print("get_data")
            if (file.exists(obj@filepath)) {
              tryCatch({
                dat = do.call(obj@readmethod, c(file = obj@filepath, obj@readparam, fill = TRUE))
                # print(paste0("obj@value: ", obj@value))
                # print(paste0("obj@missing_val: ", obj@missing_val))
                # if(!(obj@missing_val == "")) {
                #   dat2 = d[dat[,obj@value]!=obj@missing_val,]
                #   print(dat2)
                # }
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

