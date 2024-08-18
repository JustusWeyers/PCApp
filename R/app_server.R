#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @rawNamespace import(shiny, except = renderDataTable)
#' @importFrom golem get_golem_options
#' @importFrom shiny reactiveValues
#' @importFrom shinydashboard renderMenu
#'
#' @noRd

app_server <- function(input, output, session) {

  ##############################
  ### Global reactive values ###
  ##############################

  r <- shiny::reactiveValues(
    import_trigger = FALSE
  )

  # Environment
  r$ENV = Sys.getenv()

  # Language
  if (length(golem::get_golem_options("lang")) > 0) {
    lang <- golem::get_golem_options("lang")
  } else {
    lang = "en"
  }
  r$txt <- internal$apptext[[lang]]

  # tmap settings
  # tmap::tmap_mode("plot")

  #########################
  ### Server UI outputs ###
  #########################

  # Render sidebar menu
  output$sidebarmenu <- shinydashboard::renderMenu(
    render_sidebar(apptext = r$txt[c(59, 29, 54, 55, 56, 1)])
  )

  ####################
  ### Modular code ###
  ####################

    # Home server
  mod_home_server("home")

  # Database server (run first)
  mod_database_server("database_tab", r)

  # Import timeseries server
  mod_import_server("import_timeseries", r, dtype = r$txt[25])
  # Import metadata server
  mod_import_server("import_metadata", r, dtype = r$txt[26], predefined_groups = c("Metadata"))

  # Selection server
  mod_selection_server("select", r)

  # PCA server
  mod_PCA_server("PCA", r)

  # Export server
  mod_export_server("export", r)

  # ENV server
  mod_ENV_server("ENV_1")

}
