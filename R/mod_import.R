#' import UI Function
#'
#' @description UI of the import tab panels
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tabPanel uiOutput fluidPage fluidRow
#' @importFrom shiny actionButton textInput
#' @importFrom tools file_path_sans_ext

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
        # Array for data groups
        shiny::uiOutput(ns("groupArray")),
        # Add a group
        shiny::textInput(ns("groupname"), "Group name"),
        shiny::actionButton(ns("addgroup"), "Add group")
      )
    )
  )
}

#' import Server Functions
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent renderText
#' @importFrom shiny renderUI fileInput
#' @importFrom purrr lmap map pmap map_vec
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom methods new
#' @importFrom shinydashboard box
#'
#' @noRd

mod_import_server <- function(id, r, txt, dtype){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Modules central dataserver reactive values
    dataserver = shiny::reactiveValues(
      groupObjects = NULL,
      delete = c()
    )

    # Server logic

    ## Update on changes in r$primary_table
    shiny::observeEvent(c(r$primary_table, r$datagroup_table), {
      print(r$primary_table)
      print(r$datagroup_table)

      # Create data-objects from primarytables data
      ## Fetch modules data from primary_table
      data = r$primary_table[r$primary_table$dtype == dtype,]
      ## Serial instantiation
      dataObjects = purrr::pmap(
        data, function(key, name, dtype, dgroup)
        # Constructor
        return(methods::new(dtype, key = key, name = name, dtype = dtype, dgroup = dgroup))
      )

      # Create group-objects from datagroup_table
      ## Fetch modules groups from primary_table
      groups = r$datagroup_table[r$datagroup_table$dtype == dtype,]
      ## Serial instantiation
      dataserver$groupObjects = purrr::pmap(
        groups, function(key, name, dtype, color) {
        # For each group fetch group data objects
        groupdata = dataObjects[purrr::map_vec(dataObjects, function(o) o@dgroup == name)]
        # Constructor
        return(methods::new("Group", key = key, name = name, dtype = dtype, color = color, data = groupdata))
      })

      # Set names of dataserver groupData
      names(dataserver$groupObjects) = purrr::map_vec(dataserver$groupObjects, function(group) group@name)

      # Call group servers
      lapply(dataserver$groupObjects, function(o) groupServer(o, r = r, dataserver = dataserver, txt = txt))

    })

    ## Delete group on changes in dataserver$delete
    shiny::observeEvent(dataserver$delete, {
      print("Delete Group")
      # Iterate over delete queue
      lapply(dataserver$delete, function(name) {
        # Delete from database
        delete.data(r$db, dataserver$groupObjects[[name]])
        # Delete from groupdata
        dataserver$groupObjects[name] <- NULL
      })
      # Clear queue
      dataserver$delete <- c()
    })

    # Add Group
    shiny::observeEvent(input$addgroup, {

      # Check if group already exists
      if (!(input$groupname %in% names(dataserver$groupObjects))) {
        # Add entry to datagroup_table
        appendto.table(r$db, table = "datagroup_table", values = data.frame(
          name = input$groupname,
          dtype = dtype,
          color = "pink"
        ))
        # Update datagroup_table
        r$datagroup_table = get.table(r$db, tablename = "datagroup_table")
      } else {
        print("Groupname already in use")
      }

    })

    # UI Elements

    ## Render tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(dtype)
    })

    ## Render group boxes
    shiny::observeEvent(r$datagroup_table, {
      # Clear ui
      output$groupArray <- shiny::renderUI(NULL)
      # Iterate over groupObjects
      output$groupArray <- shiny::renderUI(
        lapply(dataserver$groupObjects,
               function(g) {
                 print("Render Group")
                 # Namespace
                 g@name <- ns(g@name)
                 groupUI(g)()
                }))
    })
  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
