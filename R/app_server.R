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
    import_trigger = FALSE,
    cache_selection_trigger = FALSE,
    metadata = NULL
  )

  # Environment
  r$ENV = c(
    Sys.getenv(), 
    list(SHINYPROXY_USERNAME = "Weyers", SHINYPROXY_USERGROUPS = "admin", Weyers_DBPW = "54321" )   #### DELETE ME !!!!1!1!!!!!1!
    # list(SUPER_USER = "suppi", SHINYPROXY_USERGROUPS = "suppipassword" )  #### DELETE ME !!!!1!1!!!!!1!
  )
  
  print(shiny::isolate(r$ENV))
  
  
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
  
  # User
  
  r$webmode = golem::get_golem_options("webmode")
  r$shiny_proxy = "SHINYPROXY_USERNAME" %in% names(shiny::isolate(r$ENV))
  
  if (shiny::isolate(r$webmode) & shiny::isolate(r$shiny_proxy)) {
    r$user = getElement(shiny::isolate(r$ENV), "SHINYPROXY_USERNAME")
    r$usergroup = getElement(shiny::isolate(r$ENV), "SHINYPROXY_USERGROUPS")
    r$userdbpw = getElement(shiny::isolate(r$ENV), paste0(shiny::isolate(r$user), "_DBPW"))
    r$superuser = getElement(shiny::isolate(r$ENV), "SUPER_USER")
    r$superuserPW = getElement(shiny::isolate(r$ENV), "SUPER_USER_PW")
  } else {
    r$user = getElement(shiny::isolate(r$ENV), "USERNAME")
    r$usergroup = NA_character_
    r$superuser = getElement(internal$acc, "superuser")
    r$superuserPW = getElement(internal$acc, "superpassword")
  }
  
  # Admin rights
  if (shiny::isolate(r$webmode) & identical(shiny::isolate(r$usergroup), "admin")) {
    r$admin = TRUE
  } 
  if (shiny::isolate(r$webmode) & !(identical(shiny::isolate(r$usergroup), "admin"))) {
    r$admin = FALSE
  }
  if (!(shiny::isolate(r$webmode))) {
    r$admin = TRUE
  }

  #########################
  ### Server UI outputs ###
  #########################

  # Render sidebar menu
  
  output$user = shiny::renderUI(
    
    if (r$webmode) {
      
      if (r$admin) {
        text = paste(shiny::isolate(r$user), "(Admin)")
      } else {
        text = shiny::isolate(r$user)
      }
      
      ui = shiny::fluidRow(
        col_12(
          hr(style = "border-top: 1px solid #c1c9bf;"),
          shinydashboard::sidebarMenu(
            id="user",
            sidebarmenu <- shinydashboard::sidebarMenu(
              shinydashboard::menuItem(text = text, icon = shiny::icon("user"))
            )
          )
        )
      )
      return(ui)
    }
  )
  
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
