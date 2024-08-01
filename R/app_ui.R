#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList tabsetPanel
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard sidebarMenuOutput dashboardBody tabItems
#' @importFrom shinydashboard tabItem sidebarMenu
#'
#' @noRd
app_ui <- function(request) {
  shiny::tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Dashboard structure
    shinydashboard::dashboardPage(

      # Header
      shinydashboard::dashboardHeader(title = "PCApp"),

      # Sidebar
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id="mytabs",
          shinydashboard::sidebarMenuOutput("sidebarmenu")
        )
      ),



      # Body
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(

          # Home
          shinydashboard::tabItem(
            tabName = "home",
            mod_home_ui("home")
          ),

          # Import
          shinydashboard::tabItem(
            tabName = "import", shiny::tabsetPanel(
              type = "tabs",
              mod_import_ui("import_timeseries"),
              mod_import_ui("import_metadata")
            )
          ),

          # Data selection
          shinydashboard::tabItem(
            tabName = "selection",
            mod_selection_ui("select")
          ),

          # Analysis
          shinydashboard::tabItem(
            tabName = "pca", shiny::tabsetPanel(
              type = "tabs",
              mod_PCA_ui("PCA")
            )
          ),

          # Export
          shinydashboard::tabItem(
            tabName = "export",
            mod_export_ui("export")
          ),

          # Settings
          shinydashboard::tabItem(
            tabName = "settings", shiny::tabsetPanel(
              type = "tabs",
              mod_database_ui("database_tab"),
              mod_ENV_ui("ENV_1")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "PCApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
