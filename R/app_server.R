#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @rawNamespace import(shiny, except = renderDataTable)
#' @importFrom golem get_golem_options
#' @noRd

app_server <- function(input, output, session) {
  # Application server logic

  # Global reactive values
  r <- shiny::reactiveValues()

  # Read environment
  r$ENV = Sys.getenv()

  # Fetch application language
  lang <- golem::get_golem_options("lang")

  # Read apptext (internal) depending on golem_opts 'lang'
  if (is.null(lang)) {
    txt <- internal$apptext[["en"]]
  } else {
    txt <- internal$apptext[[lang]]
  }

  # Render sidebar menu based on txt elements
  output$sidebarmenu <- render_sidebar(apptext = txt[c(29, 1)])

  # Modular code
  mod_database_server("database_tab", r, txt)
  mod_ENV_server("ENV_1")

  mod_import_server("import_timeseries", r, txt, dtype = txt[25])
  mod_import_server("import_metadata", r, txt, dtype = "Metadata")
}
