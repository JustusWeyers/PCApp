#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom golem get_golem_options
#' @noRd

app_server <- function(input, output, session) {

  # Application server logic

  # Global reactive values
  r <- shiny::reactiveValues()

  # Fetch application language
  lang = golem::get_golem_options('lang')

  # Read app text depending on golem_opts 'lang'
  if (is.null(lang)) {
    txt = apptext[["en"]]
  } else{
    txt = apptext[[lang]]
  }

  # Render sidebar menu based on txt elements
  output$sidebarmenu <- render_sidebar(apptext = txt[1])

  # Modular code
  mod_database_server('database_tab', r, txt)

}
