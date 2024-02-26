#' upload_shapefile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_shapefile_ui <- function(id){
  ns <- NS(id)
  shiny::tabPanel(
    shiny::uiOutput(ns("ui_tab_title")),
    shiny::tagList(
      shiny::fluidPage()
    )
  )
}

#' upload_shapefile Server Functions
#'
#' @noRd
mod_upload_shapefile_server <- function(id, r, txt){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(txt[27])
    })
  })
}

## To be copied in the UI
# mod_upload_shapefile_ui("upload_shapefile_1")

## To be copied in the server
# mod_upload_shapefile_server("upload_shapefile_1")
