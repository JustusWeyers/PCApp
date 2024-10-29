#' ENV UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tabPanel tagList fluidPage fluidRow uiOutput renderTable
#' @importFrom shiny renderUI
mod_ENV_ui <- function(id){
  ns <- NS(id)
  shiny::tabPanel(
    "ENV",# shiny::uiOutput(ns("ui_tab_title")),
    shiny::tagList(
      shiny::fluidPage(
        # Body
        shiny::fluidRow(
          shiny::uiOutput(ns("ENV")),
        )
      )
    )
  )
}

#' ENV Server Functions
#'
#' @noRd
mod_ENV_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ENV <- shiny::renderUI({
      shiny::renderTable(Sys.getenv(), bordered = TRUE, rownames = TRUE, colnames = FALSE)
    })

  })
}

## To be copied in the UI
# mod_ENV_ui("ENV_1")

## To be copied in the server
# mod_ENV_server("ENV_1")
