#' ts_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ts_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_12(
      fileInput(ns("upload"), NULL, buttonLabel = "Upload...", multiple = TRUE),
      tableOutput(ns("files"))
    )
  )
}

#' ts_upload Server Functions
#'
#' @noRd
mod_ts_upload_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    r$my_other_module <- reactiveValues()

    observeEvent(input$upload, {
      r$ts_upload$upload <- setNames(input$upload$datapath, input$upload$name)
    })


  })
}

## To be copied in the UI
# mod_ts_upload_ui("ts_upload_1")

## To be copied in the server
# mod_ts_upload_server("ts_upload_1")
