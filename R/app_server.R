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

  # Environment
  r$ENV = Sys.getenv()

  # Fetch application language
  lang <- golem::get_golem_options("lang")

  # Read app text depending on golem_opts 'lang'
  if (is.null(lang)) {
    txt <- apptext[["en"]]
  } else {
    txt <- apptext[[lang]]
  }

  # Render sidebar menu based on txt elements
  output$sidebarmenu <- render_sidebar(apptext = txt[c(29, 1)])

  # Modular code
  mod_upload_timeseries_server("upload_timeseries", r, txt)
  mod_upload_shapefile_server("upload_shapefile", r, txt)
  mod_upload_metadata_server("upload_metadata", r, txt)
  mod_upload_raster_server("upload_raster", r, txt)

  mod_database_server("database_tab", r, txt)
  mod_ENV_server("ENV_1")
}
