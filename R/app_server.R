#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom golem get_golem_options
#' @noRd

app_server <- function(input, output, session) {

  # Application server logic

  # Global reactive values
  r <- reactiveValues()

  # Fetch application language
  lang = golem::get_golem_options('lang')

  # Read app text depending on golem_opts 'lang'
  if (is.null(lang)) {
    txt = apptext[["en"]]
  } else{
    txt = apptext[[lang]]
  }

  # Render sidebar menu based on txt elements
  output$sidebarmenu <- render_sidebar(apptext = txt[1:3])

  # Modular code
  mod_database_server('database_1', r)

  # mod_settings_tab_server("settings_tab_db", r)
  # mod_ts_upload_server('ts_upload_1', r)
}
