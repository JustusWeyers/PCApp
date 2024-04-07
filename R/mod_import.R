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

    # Functions
    import = function(name, size, type, datapath, r, dtype) {
      # Extract name for display
      display_name = tools::file_path_sans_ext(name)
      # Combine working name as pair of displayName and datatype
      working_name = paste0(display_name, "_", dtype)
      # File extension
      ext = tools::file_ext(name)
      # Create new upload data object
      newDataObject = methods::new(
        dtype,
        dtype = dtype,
        name = working_name,
        filename = name,
        displayname = display_name,
        filepath = datapath,
        filetype = type,
        filesize = size,
        fileext = ext
      )
      # Read in data
      data = read.data(newDataObject)
      # Write data
      write.data(r$db, newDataObject, data = data)
    }

    # Server logic

    ## Update on changes in r$primary_table
    observeEvent(r$primary_table, {
      print(r$primary_table)
      # Fetch modules data from primary_table
      data = isolate(r$primary_table[r$primary_table$dtype == dtype,])
      # Create dataObjects from primarytables data
      dataserver$dataObjects = purrr::pmap(
        data, function(key, name, dtype, displayname, filename, filetype,
                       filesize, fileext)
        new(dtype, name = name)
      )
      # Call box servers
      lapply(dataserver$dataObjects, function(o) boxServer(o, dataserver, txt = txt))
    })

    ## On upload
    shiny::observeEvent(ns(input$upload), {
      print("Upload")
      # Check if file is available
      if (any(!is.null(input$upload))) {
        purrr::pmap(input$upload, import, r = r, dtype = dtype)
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
        shiny::fileInput(ns("upload"), "Upload a file", multiple = TRUE))
    )

  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
