#' import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom tools file_path_sans_ext
#'

mod_import_ui <- function(id){
  ns <- NS(id)
  shiny::tabPanel(
    shiny::uiOutput(ns("ui_tab_title")),
    shiny::tagList(
      shiny::fluidPage(
        # Body
        h1("H1"),
        shiny::fluidRow(
          shiny::uiOutput(ns("ui_boxes")),
          shiny::uiOutput(ns("ui_empty_box"))
        )
      )
    )
  )
}

#' import Server Functions
#'
#' @importFrom purrr lmap
#' @importFrom utils read.csv
#' @noRd
mod_import_server <- function(id, r, txt, dtype){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    databoxes = function(tables) {
      boxes = purrr::lmap(tables, function(name) shinydashboard::box(
        id = ns(paste0("box_", name)), title = name, width = "100%",
        collapsible = TRUE, collapsed = TRUE,
        actionButton(ns(paste0("delete_", name)), "Delete")
        ))
      return(boxes)
    }

    # Server code

    # Servers reactive values
    server = reactiveValues(
      primary_table = NULL,
      data = NULL
    )

    # Import data
    observeEvent(ns(input$upload), {
      if (!is.null(input$upload)) {
        # Extract name
        name = tools::file_path_sans_ext(input$upload$name)
        # Read file (Todo)
        df = read.csv(input$upload$datapath)
        # Write to database and create entry in database primary table
        write.dbtable(r$mod_database$db, name = name, df = df, dtype = dtype)
      }

      # Update primary table
      server$primary_table = get.table(r$mod_database$db, tablename = "primary_table")
      # Tables corresponding to modules datatype
      server$data = server$primary_table[server$primary_table$datatype == dtype,]

      print(server$primary_table)

    })

    # UI Elements

    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(dtype)
    })

    output$ui_boxes <- shiny::renderUI(
      databoxes(server$data$name)
    )

    output$ui_empty_box <- shiny::renderUI(
      shinydashboard::box(
        id = ns("empty_box"), title = paste("New", dtype), width = "100%",
        fileInput(ns("upload"), "Upload a file"))
    )

  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
