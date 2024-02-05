#' database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList verbatimTextOutput fileInput
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbWriteTable dbListTables
#' @importFrom utils read.table


mod_database_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_12(
      verbatimTextOutput(ns("db_status")),
      verbatimTextOutput(ns("filepath")),
      verbatimTextOutput(ns("db_tables"))
    )
  )
}

#' database Server Functions
#'
#' @noRd
mod_database_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Establish database connection
    con <- connect_db(hosts = c("172.22.0.2"))

    # Text output of connection status
    if (is.null(con)) output$db_status <- renderText({"No DB"})
    else output$db_status <- renderText({"DB"})

    # Write csv-file to database
    observeEvent(r$ts_upload$upload, {
      if (!is.null(con) & length(r$ts_upload$upload) >= 1) {
        print(paste("Upload:", basename(r$ts_upload$upload)))
        for (path in r$ts_upload$upload) {
          DBI::dbWriteTable(conn = con,
                            name = tools::file_path_sans_ext(names(which(
                              r$ts_upload$upload == path))),
                            value = utils::read.csv(r$ts_upload$upload),
                            overwrite = TRUE)
          r$ts_upload$upload = r$ts_upload$upload[!r$ts_upload$upload == path]
        }
        output$db_tables <- renderText(DBI::dbListTables(con))
      }
      print(paste("Upload:", basename(r$ts_upload$upload)))
    })


  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
