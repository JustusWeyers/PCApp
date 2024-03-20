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
      fluidPage(
        uiOutput(ns("boxArray")),
        shiny::uiOutput(ns("ui_empty_box"))
      )
    )
  )
}

#' import Server Functions
#'
#' @importFrom purrr lmap map
#' @importFrom utils read.csv
#' @noRd
mod_import_server <- function(id, r, txt, dtype){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### Servers reactive values
    dataserver = reactiveValues(
      primary_table = NULL,
      tables = NULL,
      boxes = NULL,
      delete = c()
    )

    ### Server logic

    # On upload
    observeEvent(ns(input$upload), {

      print("Upload Event")

      if (!is.null(input$upload)) {
        # Extract name
        name = tools::file_path_sans_ext(input$upload$name)
        # Read file (Todo: depending on different files and user parameters)
        df = read.csv(input$upload$datapath)
        # Combine table name as pair of data name and datatype
        tablename = paste0(name, "_", dtype)
        # Write to database and create entry in database primary table
        write.dbtable(r$mod_database$db, name = tablename, df = df, dtype = dtype)
      }

      # Update primary table
      dataserver$primary_table = get.table(r$mod_database$db, tablename = "primary_table")


    })

    # On changes in dataserver$primarytable
    observeEvent(dataserver$primary_table, {

      print("Change in dataserver$primarytable")
      print(dataserver$primary_table)

      # Update tables corresponding to modules datatype
      dataserver$tables = dataserver$primary_table[dataserver$primary_table$datatype == dtype,]

      # Update box objects
      dataserver$boxes = lapply(dataserver$tables$name, function(n) new(dtype, name = as.character(n)))

      # Call box server on box objects
      lapply(dataserver$boxes, function(x) boxServer(x, dataserver))
    })

    observeEvent(dataserver$delete, {

      print(paste("Delete:", dataserver$delete))

      # DBI DELETION
      lapply(dataserver$delete, function(n) delete.dbtable(r$mod_database$db, n))
      # Clear queue
      dataserver$delete <-  c()

      # Update primary_table
      dataserver$primary_table = get.table(r$mod_database$db, tablename = "primary_table")

    })

    ### UI Elements

    # Render tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(dtype)
    })

    # Call box ui
    output$boxArray = renderUI(lapply(dataserver$boxes, function(b) {
      b@name <- ns(b@name) # Important
      boxUI(b)()
    }))


    output$ui_empty_box <- shiny::renderUI(
      shinydashboard::box(
        id = ns("empty_box"), title = paste("New", dtype), width = 12,
        fileInput(ns("upload"), "Upload a file"))
    )

  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
