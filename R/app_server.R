#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom purrr map2
#' @importFrom golem get_golem_options
#' @noRd

app_server <- function(input, output, session) {

  # Read App-Text depending on golem_opts lang
  txt = apptext[[golem::get_golem_options("lang")]]

  # Define sidebar menu
  menu_items = data.frame(name = txt[1], id = "settings")

  # Build sidebar menu
  output$menu <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      purrr::map2(menu_items$name, menu_items$id,
                  \(x, y) shinydashboard::menuItem(x, tabName = y)
      )
    )
  })

  # Your application server logic
  r <- reactiveValues()
  # mod_database_server("database_1", r)
  mod_ts_upload_server("ts_upload_1", r)
}
