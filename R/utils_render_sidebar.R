#' render_sidebar
#'
#' @description Render sidebar menu based on textelements
#'
#' @return sidebarmenu A shinydashboard::renderMenu()-menu
#'
#' @noRd
#'
#' @importFrom shinydashboard renderMenu sidebarMenu menuItem
#' @importFrom purrr pmap
#' @importFrom shiny icon

render_sidebar <- function(apptext) {
  # Define sidebar menu content
  menu_items <- data.frame(name = apptext,
                           id = c("home", "import",  "selection", "pca", "export", "settings"),
                           icon = c("house", "file-arrow-up", "filter", "wave-square", "file-arrow-down", "gear"))

  # Build sidebar menu
  sidebarmenu <- shinydashboard::renderMenu(
    shinydashboard::sidebarMenu(
      # Iterate over menu_items name and id
      purrr::pmap(
        list(menu_items$name, menu_items$id, menu_items$icon),
        \(x, y, i) shinydashboard::menuItem(x, tabName = y, icon = shiny::icon(i))
      )
    )
  )

  return(sidebarmenu)
}
