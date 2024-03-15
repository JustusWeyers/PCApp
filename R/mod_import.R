#' import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_ui <- function(id){
  ns <- NS(id)
  shiny::tabPanel(
    "Title", # shiny::uiOutput(ns("ui_tab_title")),
    shiny::tagList(
      shiny::fluidPage(
        # Body
        h1("H1"),
        shiny::fluidRow(
          shiny::uiOutput(ns("ui_boxes"))
        )
      )
    )
  )
}

#' import Server Functions
#'
#' @importFrom purrr lmap
#' @noRd
mod_import_server <- function(id, r, txt, title){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(title)
    })

    databox = function(n) {
      boxes = purrr::lmap(n, function(i) shinydashboard::box(
        id = ns(paste0("box_", i)), title = toString(i), width = "100%",
        shiny::textOutput("Hello there")))
      return(boxes)
    }

    output$ui_boxes <- shiny::renderUI(
      databox(1:100)
    )

  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
