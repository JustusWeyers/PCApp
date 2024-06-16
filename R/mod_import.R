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
        shiny::uiOutput(ns("ui_groupArray")),
        # Add a group
        shiny::uiOutput(ns("ui_addagroup"))
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

mod_import_server <- function(id, r, txt, dtype, predefined_groups = c()){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive values

    importserver = shiny::reactiveValues(
      predefined_groups = predefined_groups,
      groupObjects = NULL,
      delete = c()
    )

    # Functions

    check_predefined_groups = reactive({
      length(importserver$predefined_groups) > 0
    })

    ## Fetch datagroup_table group infos
    groups = reactive(r$datagroup_table[r$datagroup_table$dtype == dtype,])

    ## Create groups from datagroup table
    dataGroups = reactive({
      # Serial instantiation
      grouplist = purrr::pmap(
        .l = groups(),
        .f = function(key, name, dtype, color, readmethod) {
        # Constructor
        methods::new(
          "Group",
          key = key,
          name = name,
          dtype = dtype,
          color = color,
          readmethod = readmethod
        )
      })
      # Set names of grouplist
      names(grouplist) = purrr::map_vec(grouplist, function(group) group@name)
      return(grouplist)
    })

    # Call Group servers
    call_group_servers = function(group_objects) {
      lapply(
        group_objects,
        function(o) groupServer(o, r = r, importserver = importserver, txt = txt)
      )
    }

    # Server logic

    ## Update on changes in r$primary_table or r$datagroup_table
    shiny::observeEvent(r$datagroup_table, {
      # (Re-) Create Groups
      importserver$groupObjects <- dataGroups()
      # Call group servers
      call_group_servers(importserver$groupObjects)
    })

    ## Delete group on changes in importserver$delete
    shiny::observeEvent(importserver$delete, {
      print("Delete Group")
      # Iterate over delete queue
      lapply(importserver$delete, function(n) {
        # Delete from database
        delete.data(r$db, importserver$groupObjects[[n]])
        # Delete from groupdata
        importserver$groupObjects[n] <- NULL
      })
      # Clear queue
      importserver$delete <- c()
    })

    # Add predefined groups
    shiny::observeEvent(
      eventExpr = importserver$predefined_groups,
      handlerExpr = {
        # Check if there are (already existing) predefined groups
        if (check_predefined_groups()) {
          lapply(importserver$predefined_groups, function(gn) {
            if (!(gn %in% names(importserver$groupObjects))) {
              appendto.table(
                d = r$db,
                table = "datagroup_table",
                values = data.frame(
                  name = gn,
                  dtype = dtype,
                  color = "grey",
                  readmethod = methods::new(dtype)@readmethod
                )
              )
              # Update datagroup_table
              r$datagroup_table = get.table(r$db, tablename = "datagroup_table")
            }
          })
        }
    })

    ## Add user groups
    shiny::observeEvent(
      eventExpr = input$addgroup,
      handlerExpr = {
        # Check if group already exists
        if (!(input$groupname %in% c(names(importserver$groupObjects), ""))) {
          # If groupname is valid add group to datagroup_table
          appendto.table(
            d = r$db,
            table = "datagroup_table",
            values = data.frame(
              name = input$groupname,
              dtype = dtype,
              color = sample(grDevices::colors(), 1),
              readmethod = methods::new(dtype)@readmethod
            )
          )
          # Update datagroup_table
          r$datagroup_table = get.table(r$db, tablename = "datagroup_table")
        } else {
          print("Groupname is not valid")
        }
      }
    )

    # UI Elements

    ## Render tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(dtype)
    })

    ## Render group boxes
    shiny::observeEvent(
      eventExpr = importserver$groupObjects,
      handlerExpr = {
        # Clear group array ui
        output$ui_groupArray <- shiny::renderUI(NULL)
        # Iterate over groupObjects
        output$ui_groupArray <- shiny::renderUI(
          lapply(
            X = importserver$groupObjects,
            FUN = function(g) {
              # Namespace
              g@name <- ns(g@name)
              groupUI(g)()
            }
          )
        )
      }
    )

    ## Render section to add a group
    output$ui_addagroup <- shiny::renderUI(
      if (!(check_predefined_groups())) {
        fluidRow(
          col_4(
            # Column content
            shiny::textInput(
              # Text input parameters
              inputId = ns("groupname"),
              label = "Group name",
              width = "100%"
            )
          ),
          col_2(
            # Column parameters
            style = "margin-top: 25px;",
            # Column content
            shiny::actionButton(
              inputId = ns("addgroup"),
              label = "Add group"
            )
          ),
          col_6()
        )
      }
    )

  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
