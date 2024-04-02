#' import UI Function
#'
#' @description UI of the import tab panels
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tabPanel uiOutput fluidPage
#' @importFrom tools file_path_sans_ext
#'

mod_import_ui <- function(id){
  ns <- NS(id)
  # Tab panel
  shiny::tabPanel(
    # Tab title
    shiny::uiOutput(ns("ui_tab_title")),
    # Body elements
    shiny::tagList(
      shiny::fluidPage(
        h1(),
        # Array of boxes representing data
        shiny::uiOutput(ns("boxArray")),
        # Upload box
        shiny::uiOutput(ns("ui_empty_box"))
      )
    )
  )
}

#' import Server Functions
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent renderText
#' @importFrom shiny renderUI fileInput
#' @importFrom purrr lmap map
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom methods new
#' @importFrom shinydashboard box
#' @noRd
mod_import_server <- function(id, r, txt, dtype){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Modules central dataserver reactive values
    dataserver = shiny::reactiveValues(
      dataObjects = NULL,
      delete = c()
    )

    # Server logic

    ## Update on changes in r$primary_table
    observeEvent(r$primary_table, {
      # Fetch modules data from primary_table
      data = isolate(r$primary_table[r$primary_table$datatype == dtype,])
      # Create dataObjects from primarytables data
      dataserver$dataObjects = pmap(data, function(key, name, datatype) new(dtype, name = name, dataType = datatype))
      # Call box servers
      lapply(dataserver$dataObjects, function(o) boxServer(o, dataserver, txt = txt))
    })

    ## On upload
    shiny::observeEvent(ns(input$upload), {
      print("Upload")
      # Check if file is available
      if (!is.null(input$upload)) {
        # Fetch filename
        filename = input$upload$name
        # Fetch filepath
        filepath = input$upload$datapath
        # Extract name for display
        displayName = tools::file_path_sans_ext(filename)
        # Extract filetype
        filetype = tools::file_ext(filename)
        # Combine working name as pair of displayName and datatype
        name = paste0(displayName, "_", dtype)
        # Create Data object
        newDataObject = methods::new(
          dtype,
          name = name,
          filename = filename,
          filepath = filepath,
          displayName = displayName,
          dataType = dtype,
          filetype = filetype
        )
        # Read in data
        data = read.data(newDataObject)
        # Write to database and create entry in database primary table
        write.dbtable(r$db, name = name, df = data, dtype = dtype)
      }
      # Update primary table
      r$primary_table = get.table(r$db, tablename = "primary_table")
    })

    ## On changes in dataserver$delete
    shiny::observeEvent(dataserver$delete, {
      print("Delete")
      # Delete elements from database
      lapply(dataserver$delete, function(name) delete.dbtable(r$db, name))
      # Update primary_table
      r$primary_table = get.table(r$db, tablename = "primary_table")
      # Clear queue
      dataserver$delete <-  c()
    })

    # UI Elements

    ## Render tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(dtype)
    })

    ## Call box ui
    observeEvent(c(r$db, r$primary_table), {
      output$boxArray <- renderUI(NULL)
      output$boxArray = renderUI(lapply(dataserver$dataObjects, function(d) {
        d@name <- ns(d@name) # Important
        boxUI(d)()
      }))
    })

    ## Upload box
    output$ui_empty_box <- shiny::renderUI(
      shinydashboard::box(
        id = ns("empty_box"), title = paste("New", dtype), width = 12,
        shiny::fileInput(ns("upload"), "Upload a file"))
    )

  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
