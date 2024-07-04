#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @rawNamespace import(shiny, except = renderDataTable)
#' @importFrom golem get_golem_options
#' @noRd

app_server <- function(input, output, session) {

  ##############################
  ### Global reactive values ###
  ##############################

  r <- shiny::reactiveValues()

  # Environment
  r$ENV = Sys.getenv()

  # Language
  lang <- golem::get_golem_options("lang")
  if (is.null(lang)) {
    txt <- internal$apptext[["en"]]
  } else {
    txt <- internal$apptext[[lang]]
  }

  # Table for raw data
  r$rawtable = setNames(data.frame(matrix(ncol = 1, nrow = 0)), "timestamp")

  #########################
  ### Server UI outputs ###
  #########################

  # Render sidebar menu
  output$sidebarmenu <- render_sidebar(apptext = txt[c(59, 29, 54, 55, 56, 1)])

  ####################
  ### Modular code ###
  ####################

  mod_home_server("home")

  mod_database_server("database_tab", r, txt)
  mod_ENV_server("ENV_1")

  mod_import_server("import_timeseries", r, txt, dtype = txt[25])
  mod_import_server("import_metadata", r, txt, dtype = txt[26], predefined_groups = c("Metadata"))
  mod_selection_server("select", r, txt)
  mod_PCA_server("PCA", r, txt)
  mod_export_server("export", r, txt)
}
