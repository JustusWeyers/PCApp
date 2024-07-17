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



              ########################
              ### Server functions ###
              ########################

              observeEvent(file_path(), {
                if (!is.null(file_path())){
                  print("# Metadata upload")
                  write.dbtable(r$db, metadata_server$obj@name, data.frame(data = stringi::stri_trans_general(readLines(file_path()), "Latin-ASCII")))
                  metadata_server$dparam["filepath"] <- NULL
                }
              })

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
                  }
                })
                print("--- finish ids ---")
              })

              # Communication
              observeEvent(c(group_server$read_options, group_server$group_options), {
                print("MS observed group server read options")
                metadata_server$gparam <- get_gparam()
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

              output$ui_table_head = DT::renderDataTable({
                data()
              })

              # Create delete Button
              output$ui_delete_button <- shiny::renderUI(
                shiny::actionButton(ns(paste0(RANDOMADDRESS, "_delete_button")), label = r$txt[32], width = "100%")
              )

              output$ui_databox = shiny::renderUI({
                shinydashboard::box(
                  id = "box", title = metadata_server$obj@name, width = 12,
                  collapsible = TRUE, collapsed = TRUE,
                  shiny::fluidRow(
                    col_12(
                      DT::dataTableOutput(ns("ui_table_head")),
                    )
                  ),
                  shiny::fluidRow(
                    col_6(
                      shiny::uiOutput(ns("ui_option_box"))
                    ),
                    col_2(

                    ),
                    col_4(
                      shiny::uiOutput(ns("ui_delete_button"))
                    )
                  )
                )
              })

              ####

            })
            return(server)
          })
