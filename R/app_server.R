#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  mod_database_server("database_1", r)
  mod_ts_upload_server("ts_upload_1", r)
}
