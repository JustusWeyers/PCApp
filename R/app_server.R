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

  r$settings = list(
    crs = 25833,
    flip_pca = FALSE
  )

  # Language
  if (length(golem::get_golem_options("lang")) > 0) {
    r$lang <- golem::get_golem_options("lang")
  } else {
    r$lang = "en"
  }

  observeEvent(r$lang, {
    r$txt <- internal$apptext[[r$lang]]
  })

  # Plots
  r$plots = list()
  
  # Admin rights
  if (golem::get_golem_options("webmode")) {
    if ("SHINYPROXY_USERGROUPS" %in% names(shiny::isolate(r$ENV))) {
      usergroup = getElement(r$ENV, "SHINYPROXY_USERGROUPS")
      if (identical(tolower(usergroup), "admin")) {
        r$admin = TRUE
      } else {
        r$admin = FALSE
      }
    } else {
      r$admin = FALSE
    }
  } else {
    r$admin = TRUE
  }

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

  n = 9
  shiny::withProgress(message = "Starting up", value = 0, {
    # Home server
    shiny::incProgress(1/n, detail = "load home module")
    mod_home_server("home", r)

    # Database server (run first)
    shiny::incProgress(1/n, detail = "load database module")
    mod_database_server("database_tab", r)

    # Import modules
    shiny::incProgress(1/n, detail = "load import module timeseries")
    mod_import_server("import_timeseries", r, dtype = "Timeseries")
    shiny::incProgress(1/n, detail = "load import module metadata")
    mod_import_server("import_metadata", r, dtype = "Metadata", predefined_groups = c("Metadata"))
    shiny::incProgress(1/n, detail = "load import module vector data")
    mod_import_server("import_vectordata", r, dtype = "VectorData", predefined_groups = c("Untersuchungsgebiet"))
    # mod_import_server("import_rasterdata", r, dtype = "RasterData")
    # shiny::incProgress(1/n, detail = "load import module raster data")

    # Selection server
    shiny::incProgress(1/n, detail = "load select module")
    mod_selection_server("select", r)
    
    # PCA server
    shiny::incProgress(1/n, detail = "load PCA module")
    mod_PCA_server("PCA", r)
    mod_PCA_PCA_server("PCA-PCA", r)
    mod_PCA_Component_loadings_server("PCA-Component_loadings", r)
    mod_PCA_Pairing_of_component_loadings_server("PCA-Pairing_of_component_loadings", r)
    mod_PCA_Combinations_of_principal_components_server("PCA-Combinations_of_principal_components", r)
    mod_PCA_Linear_regression_server("PCA-Linear_regression", r)
    
    # mod_PCA_Correlations_server("PCA-Correlations", r)

    # Export server
    shiny::incProgress(1/n, detail = "load export module")
    mod_export_server("export", r)

    shiny::incProgress(1/n, detail = "settings")
    mod_general_server("general_settings", r)
    # ENV server
    if (shiny::isolate(r$admin)) {
      mod_ENV_server("ENV")
    }
    
  })

}
