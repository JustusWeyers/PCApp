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
           cc = "list",
           dparam = "list",
           readmethod = "character",
           readmethods = "character",
           rparam = "list"
         ),
         prototype = list(
           cc = list(
             id = "col_select",
             name = "col_select",
             longitude = "col_select",
             latitude = "col_select"
           ),
           dparam = list(),
           readmethod = c(
             "read.table"
           ),
           readmethods = c(
             "read.csv2",
             "read.csv",
             "read.table"
           ),
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
                shiny::uiOutput(ns("ui_databox"))

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

              ##################
              ### Connstants ###
              ##################

              RANDOMADDRESS = random_address()

              #################
              ### Functions ###
              #################

              getmydata = function(d, name) {
                if (paste0(name, "_clean") %in% user.tables(d)$tablename) {
                  return(get.table(d, paste0(name, "_clean")))
                } else {
                  return(data.frame())
                }
              }

              get_gparam = function() {
                dgt = get.table(shiny::isolate(r$db), "datagroup_table")
                return(as.list(jsonlite::fromJSON(dgt[dgt$key == obj@dgroup, "gparam"])))
              }

              #######################
              ### Reactive Values ###
              #######################

              metadata_server = reactiveValues(
                obj = obj,
                dparam = obj@dparam,
                gparam = get_gparam(),
                data = getmydata(r$db, obj@name)
              )

              ##########################
              ### Reactive functions ###
              ##########################

              # gparam = reactive(
              #   metadata_server$gparam
              # )



              # data_colnames <- reactive(
              #   colnames(data())
              # )
              #
              # id_column <- reactive(
              #   gparam()[["id"]]
              # )
              #
              # name_column <- reactive(
              #   gparam()[["name"]]
              # )
              #
              # longitude <- reactive(
              #   gparam()[["longitude"]]
              # )
              #
              # latitude <- reactive(
              #   gparam()[["latitude"]]
              # )
              #
              # ids = reactive({
              #   if (!is.null(id_column()) & !is.null(data_colnames())) {
              #     if (id_column() %in% data_colnames()) {
              #       return(unique(data()[,id_column()]))
              #     } else {
              #       return(NULL)
              #     }
              #   }
              # })
              #
              # clear_names = reactive({
              #   if (!is.null(name_column()) & !is.null(data_colnames())) {
              #     if (name_column() %in% data_colnames()) {
              #       return(unique(data()[,name_column()]))
              #     } else {
              #       return(NULL)
              #     }
              #   }
              # })
              #
              # lat = reactive({
              #   if (!is.null(latitude()) & !is.null(data_colnames())) {
              #     if (latitude() %in% data_colnames()) {
              #       return(unique(data()[,latitude()]))
              #     } else {
              #       return(NULL)
              #     }
              #   }
              # })
              #
              # lon = reactive({
              #   if (!is.null(longitude()) & !is.null(data_colnames())) {
              #     if (longitude() %in% data_colnames()) {
              #       return(unique(data()[,longitude()]))
              #     } else {
              #       return(NULL)
              #     }
              #   }
              # })

              ########################
              ### Server functions ###
              ########################

              observeEvent(r$import_trigger, {
                metadata_server$data <- getmydata(r$db, obj@name)
              })

              # ## _. Observe changes in group parameters
              # observeEvent(metadata_server$dparam, {
              #   # Convert gparam list into json
              #   st = toString(jsonlite::toJSON(metadata_server$dparam))
              #   # Encoding stuff
              #   st = gsub("'", "", st)
              #   # Write to database
              #   change.tablevalue(r$db, "primary_table", metadata_server$obj@key, "dparam", st)
              # })
              #
              # observeEvent(ids(), {
              #   print("--- Observe ids() ---------")
              #   pt = get.table(r$db, "primary_table")
              #
              #   purrr::map2(pt$key, pt$head, function(k, h) {
              #     check = sapply(ids(), grepl, toString(h))
              #     if (any(check)) {
              #       change.tablevalue(r$db, "primary_table", k, "id", toString(ids()[which(check)]))
              #       change.tablevalue(r$db, "primary_table", k, "clearname", toString(clear_names()[which(check)]))
              #       change.tablevalue(r$db, "primary_table", k, "lat", toString(lat()[which(check)]))
              #       change.tablevalue(r$db, "primary_table", k, "lon", toString(lon()[which(check)]))
              #     }
              #   })
              # })
              #
              # observeEvent(ids(), {
              #   r$primary_table <- get.table(r$db, "primary_table")
              # })


              ##########
              ### UI ###
              ##########

              output$ui_table_head = shiny::renderUI({
                return(DT::renderDataTable(metadata_server$data, options = list(scrollX = TRUE)))
              })

              output$ui_databox = shiny::renderUI({
                shinydashboard::box(
                  solidHeader = TRUE, width = 12,
                  shiny::fluidRow(
                    col_12(
                      shiny::uiOutput(ns("ui_table_head"))
                      # style = "overflow-x: scroll;"
                    )
                  )
                )
              })

              ####

            })
            return(server)
          })

setMethod("data_wrangling",
          methods::signature(dataobject = "Metadata"),
          function(dataobject, db, options) {
            indata = mydata(db, dataobject@name, options[["readmethod"]], options)
            write.dbtable(db, paste0(dataobject@name, "_readin"), indata)
            # hdata = head_data(db, dataobject@name, options[["readmethod"]], options)
            # write.dbtable(db, paste0(dataobject@name, "_head"), hdata)

            return(colnames(indata))
          })

# Has to be placed here instead of inside fct_classMetadata for some reason ...
setMethod("clean_data",
          methods::signature(dataobject = "Metadata"),
          function (dataobject, db, options) {
            tryCatch(expr = {
              data = get.table(db, paste0(dataobject@name, "_readin"))
              central_cnms = unlist(options)[unname(unlist(options)) %in% colnames(data)]
              df = dplyr::relocate(data, unname(central_cnms))
              colnames(df)[1:length(central_cnms)] = names(central_cnms)
              return(df)
            }, error = function(e) {
              print(e)
              return(data.frame())
            })
          })

setMethod("initial_read_write",
          methods::signature(dataobject = "Metadata"),
          function(dataobject, db) {

            write.dbtable(
              db,
              dataobject@name,
              data.frame(
                data = stringi::stri_trans_general(
                  readLines(dataobject@dparam[["filepath"]]), "Latin-ASCII")
              )
            )

          })
