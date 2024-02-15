#' ts_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tableOutput fileInput
#' @importFrom stats setNames
#'
mod_ts_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    col_12(
      shiny::fileInput(ns("upload"), NULL, buttonLabel = "Upload...", multiple = TRUE),
      shiny::tableOutput(ns("files"))
    )
  )
}

#' ts_upload Server Functions
#'
#' @importFrom shiny moduleServer observeEvent
#'
#' @noRd
mod_ts_upload_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$upload, {
      r$ts_upload$upload <- setNames(input$upload$datapath, input$upload$name)
    })
  })
}

## To be copied in the UI
# mod_ts_upload_ui("ts_upload_1")

## To be copied in the server
# mod_ts_upload_server("ts_upload_1")
