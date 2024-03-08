#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_timeseries_ui <- function(id){
  ns <- NS(id)
  shiny::tabPanel(
    shiny::uiOutput(ns("ui_tab_title")),
    shiny::tagList(
      shiny::fluidPage()
    )
  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_timeseries_server <- function(id, r, txt){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(txt[25])
    })
  })
}

## To be copied in the UI
# mod_upload_timeseries_ui("upload_timeseries")

## To be copied in the server
# mod_upload_timeseries_server("upload_timeseries")
