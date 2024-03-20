#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList tabsetPanel tabPanel h1
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard sidebarMenuOutput dashboardBody tabItems
#' @importFrom shinydashboard tabItem
#'
#' @noRd
app_ui <- function(request) {
  shiny::tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboard::dashboardPage(

      # Header
      shinydashboard::dashboardHeader(title = "PCApp"),

      # Sidebar
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenuOutput("sidebarmenu")
      ),

      # Body
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(

          # Import
          shinydashboard::tabItem(
            tabName = "import", shiny::tabsetPanel(
              type = "tabs",
              mod_import_ui("import_timeseries"),
            )
          ),

          # Analysis

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
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
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
