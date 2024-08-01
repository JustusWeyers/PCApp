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
             # valid_column = "col_select",
             # valid_status = "checkbox_input",
             # colnames = NA_character_,
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

              data = function(d, name) {
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


              data_colnames <- reactive(
                colnames(data(r$db, obj@name))
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
                if (!is.null(timeseries_server$data)) {
                  d = timeseries_server$data
                  colnames(d) = c("timestamp", "value")
                  ggplot2::ggplot(data = d, ggplot2::aes(x = timestamp, y = value)) +
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
              #
              # lat = reactive({
              #   tryCatch({
              #     as.numeric(meta_infos()[["lat"]])
              #   }, error = function(e) {
              #     return(NULL)
              #   })
              # })
              #
              # lon = reactive({
              #   tryCatch({
              #     as.numeric(meta_infos()[["lon"]])
              #   }, error = function(e) {
              #     return(NULL)
              #   })
              # })
              #
              # shp = reactive(
              #   sf::st_as_sf(
              #     data.frame(
              #       name = meta_infos()[["clearname"]],
              #       lat = lat(),
              #       lon = lon()),
              #     coords = c("lon","lat"),
              #     crs = sf::st_crs(25832))
              # )
              #
              # location_plot = reactive({
              #   leaflet::leaflet(data = sf::st_transform(shp(), '+proj=longlat +datum=WGS84')) |>
              #     leaflet::addTiles() |>
              #     leaflet::addCircleMarkers()
              # })
              #
              #
              # ########################
              # ### Server functions ###
              # ########################

              print("TS Server is on air")

              observeEvent(group_server$trigger, {
                timeseries_server$data <- data(r$db, obj@name)
                timeseries_server$headdata <- head_data(r$db, obj@name)
              })

              #
              # observeEvent(file_path(), {
              #   if (!is.null(file_path())){
              #     shiny::showNotification(paste("Upload", timeseries_server$dparam$filename), type = "message")
              #     write.dbtable(r$db, timeseries_server$obj@name, data.frame(data = stringi::stri_trans_general(readLines(file_path()), "Latin-ASCII")))
              #     timeseries_server$dparam["filepath"] <- NULL
              #   }
              # })
              #
              # observeEvent(head_string(), {
              #   st = stringr::str_trunc(head_string(), 9999)
              #   change.tablevalue(r$db, "primary_table", timeseries_server$obj@key, "head", st)
              # })
              #
              #
              # ## _. Observe changes in group parameters
              # observeEvent(timeseries_server$dparam, {
              #   # Convert gparam list into json
              #   st = toString(jsonlite::toJSON(timeseries_server$dparam))
              #   # Encoding stuff
              #   st = gsub("'", "", st)
              #   # Write to database
              #   change.tablevalue(r$db, "primary_table", timeseries_server$obj@key, "dparam", st)
              # })
              #
              # # Communication
              # observeEvent(clean_data(), {
              #   print("TS Merge")
              #   insert_timeseries(r$db, timeseries_server$obj@name, clean_data())
              # })
              #
              # observeEvent(c(group_server$read_options, group_server$group_options), {
              #   print("TS observed gparam")
              #   timeseries_server$gparam <- get_gparam()
              # })
              #
              # observeEvent(c(group_server$read_options, group_server$group_options), {
              #   print("TS columns")
              #   group_server$columnnames <- unique(c(group_server$columnnames, data_colnames()))
              # })
              #

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

              output$ui_location_plot <- leaflet::renderLeaflet(
                location_plot()
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
