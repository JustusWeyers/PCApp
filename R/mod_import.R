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
        shiny::uiOutput(ns("ui_group_array")),
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

mod_import_server <- function(id, r, dtype, predefined_groups = c()){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    ####

    #################
    ### Functions ###
    #################

    # Fetch group objects from database
    get_group_objects = function () {
      print("Get groups")
      dgt = get.table(shiny::isolate(r$db), "datagroup_table")
      dgt = dgt[dgt$dtype == dtype,]
      go = lapply(dgt$key, function(key) {
        methods::new(
          Class = "Group",
          key = key,
          name = dgt[dgt$key == key, "name"],
          dtype = dgt[dgt$key == key, "dtype"],
          gparam = as.list(jsonlite::fromJSON(dgt[dgt$key == key, "gparam"]))
        )
      })
      # Set names of groupobjects
      go <-stats::setNames(go, dgt$name)
      # Return fresh group objects
      return(go)
    }

    ########################
    ### Server functions ###
    ########################

    # 1. Setup reactive values
    import_server = shiny::reactiveValues(
      predefined_groups = predefined_groups,
      # group_objects = shiny::isolate(get_group_objects()),
      delete_groups = NULL
    )

    # 2. Call group objects
    shiny::observeEvent(
      eventExpr = import_server$group_objects,
      handlerExpr = {
        # Call group servers
        lapply(import_server$group_objects, function(o) {
          groupServer(o, r = r, import_server = import_server)
        })
    })

    # 3.1. Observe "Add group"-button
    shiny::observeEvent(
      eventExpr = input$addgroup,
      handlerExpr = {
        print("Add a  group")
        # Check if group already exists
        if (!(input$groupname %in% c(names(import_server$group_objects), ""))) {
          # If groupname is valid add group to datagroup_table
          appendto.table(
            d = r$db,
            table = "datagroup_table",
            values = data.frame(
              name = input$groupname,
              dtype = dtype,
              gparam = jsonlite::toJSON(list(color = "#BEBEBE"))
            )
          )
          # Update import_server$group_objects
          import_server$group_objects = get_group_objects()
        } else {
          shiny::showNotification(paste(input$groupname, "already exists"), type = "error")
        }
    })

    # 3.2. Observe predefined groups
    observeEvent(import_server$predefined_groups, {
      lapply(import_server$predefined_groups, function(n) {
        if (!(n %in% get.table(r$db, "datagroup_table")$name)) {
          # If groupname is valid add group to datagroup_table
          appendto.table(
            d = r$db,
            table = "datagroup_table",
            values = data.frame(
              name = n,
              dtype = dtype,
              gparam = jsonlite::toJSON(list(color = "grey"))
            )
          )
          # Update import_server$group_objects
          import_server$group_objects = get_group_objects()
        }
      })
    })

    # 4. Delete groups
    shiny::observeEvent(
      eventExpr = import_server$delete_groups,
      handlerExpr = {
        # Delete groups in delete queue
        lapply(import_server$delete_groups, function(n) {
          # Delete from database
          delete.data(r$db, import_server$group_objects[[n]])
          # Delete from groupdata
          import_server$group_objects[n] <- NULL
        })

        r$import_trigger = !(r$import_trigger)

        # Clear delete queue
        import_server$delete_groups <- c()
    })


    ##########
    ### UI ###
    ##########

    ## Render tab title
    output$ui_tab_title <- shiny::renderUI({
      shiny::renderText(dtype)
    })

    # Group box array
    output$ui_group_array <- shiny::renderUI({
      import_server$group_objects = get_group_objects()

      lapply(
        X = import_server$group_objects,
        FUN = function(g) {
          # Namespace
          g@name <- ns(g@name)
          return(groupUI(g)())
        }
      )
    })

    ## Render section to add a group
    output$ui_addagroup <- shiny::renderUI(
      if (length(import_server$predefined_groups) == 0) {
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

    ####

  })
}

## To be copied in the UI
# mod_import_ui("import_1")

## To be copied in the server
# mod_import_server("import_1")
