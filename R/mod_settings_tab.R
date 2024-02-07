#' settings_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 moduleServer
mod_settings_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Hi!")
  )
}

#' settings_tab Server Functions
#'
#' @noRd
mod_settings_tab_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_settings_tab_ui("settings_tab_1")

## To be copied in the server
# mod_settings_tab_server("settings_tab_1")
