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
             "read.table",
             "read.csv",
             "read.csv2"
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
                gparam = get_gparam()
              )

              ##########################
              ### Reactive functions ###
              ##########################

              gparam = reactive(
                metadata_server$gparam
              )

              file_path = reactive(
                metadata_server$dparam[["filepath"]]
              )

              data = reactive({
                rm = gparam()[["readmethod"]]
                p = gparam()[names(gparam()) %in% names(optional_fun_param(rm))]
                p[["fill"]] <- TRUE
                p[["text"]] <- get.table(r$db, metadata_server$obj@name)$data

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

              id_column <- reactive(
                gparam()[["id"]]
              )

              name_column <- reactive(
                gparam()[["name"]]
              )

              longitude <- reactive(
                gparam()[["longitude"]]
              )

              latitude <- reactive(
                gparam()[["latitude"]]
              )

              ids = reactive({
                if (!is.null(id_column()) & !is.null(data_colnames())) {
                  if (id_column() %in% data_colnames()) {
                    return(unique(data()[,id_column()]))
                  } else {
                    return(NULL)
                  }
                }
              })

              clear_names = reactive({
                if (!is.null(name_column()) & !is.null(data_colnames())) {
                  if (name_column() %in% data_colnames()) {
                    return(unique(data()[,name_column()]))
                  } else {
                    return(NULL)
                  }
                }
              })

              lat = reactive({
                if (!is.null(latitude()) & !is.null(data_colnames())) {
                  if (latitude() %in% data_colnames()) {
                    return(unique(data()[,latitude()]))
                  } else {
                    return(NULL)
                  }
                }
              })

              lon = reactive({
                if (!is.null(longitude()) & !is.null(data_colnames())) {
                  if (longitude() %in% data_colnames()) {
                    return(unique(data()[,longitude()]))
                  } else {
                    return(NULL)
                  }
                }
              })

              ########################
              ### Server functions ###
              ########################

              ## _. Observe changes in group parameters
              observeEvent(metadata_server$dparam, {
                # Convert gparam list into json
                st = toString(jsonlite::toJSON(metadata_server$dparam))
                # Encoding stuff
                st = gsub("'", "", st)
                # Write to database
                change.tablevalue(r$db, "primary_table", metadata_server$obj@key, "dparam", st)
              })

              observeEvent(ids(), {
                print("--- Observe ids() ---------")
                pt = get.table(r$db, "primary_table")

                purrr::map2(pt$key, pt$head, function(k, h) {
                  check = sapply(ids(), grepl, toString(h))
                  if (any(check)) {
                    change.tablevalue(r$db, "primary_table", k, "id", toString(ids()[which(check)]))
                    change.tablevalue(r$db, "primary_table", k, "clearname", toString(clear_names()[which(check)]))
                    change.tablevalue(r$db, "primary_table", k, "lat", toString(lat()[which(check)]))
                    change.tablevalue(r$db, "primary_table", k, "lon", toString(lon()[which(check)]))
                  }
                })
              })

              observeEvent(ids(), {
                r$primary_table <- get.table(r$db, "primary_table")
              })


              ##########
              ### UI ###
              ##########

              output$ui_databox = shiny::renderUI({
                shinydashboard::box(
                  solidHeader = TRUE, width = 12,
                  shiny::fluidRow(
                    col_12(
                      DT::dataTableOutput(ns("ui_table_head")),
                    )
                  ),
                  shiny::fluidRow(
                    col_6(
                      DT::renderDataTable({
                        data()
                      })
                    ),
                    col_2(

                    )
                  )
                )
              })

              ####

            })
            return(server)
          })
