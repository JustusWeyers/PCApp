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

  # waiter::autoWaiter(id = NULL, html = NULL, color = NULL, image = "", fadeout = FALSE)

  r <- shiny::reactiveValues(
    import_trigger = FALSE,
    cache_selection_trigger = FALSE,
    metadata = NULL
  )

  # Environment
  r$ENV = Sys.getenv()

  # Language
  if (length(golem::get_golem_options("lang")) > 0) {
    r$lang <- golem::get_golem_options("lang")
  } else {
    r$lang = "en"
  }

  observeEvent(r$lang, {
    r$txt <- internal$apptext[[r$lang]]
  })

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
  mod_import_server("import_timeseries", r, dtype = "Timeseries")
  # Import metadata server
  mod_import_server("import_metadata", r, dtype = "Metadata", predefined_groups = c("Metadata"))

  mod_import_server("import_vectordata", r, dtype = "VectorData", predefined_groups = c("Untersuchungsgebiet", "Einzugsgebiete"))
  mod_import_server("import_rasterdata", r, dtype = "RasterData")



  # Selection server
  mod_selection_server("select", r)

  # PCA server
  mod_PCA_server("PCA", r)

  # Export server
  mod_export_server("export", r)

  mod_general_server("general_settings", r)
  # ENV server
  mod_ENV_server("ENV_1")

}
