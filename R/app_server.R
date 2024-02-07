#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactiveValues
#' @importFrom golem get_golem_options
#' @noRd

app_server <- function(input, output, session) {

  # Application server logic

  # Global reactive values
  r <- reactiveValues()

  # Read app text depending on golem_opts 'lang'
  txt = apptext[[golem::get_golem_options('lang')]]

  # Render sidebar menu based on txt elements
  output$sidebarmenu <- render_sidebar(apptext = txt[1:3])

  # mod_database_server('database_1', r)
  # mod_ts_upload_server('ts_upload_1', r)
}
