#' render_sidebar
#'
#' @description Render sidebar menu based on textelements
#'
#' @return sidebarmenu A shinydashboard::renderMenu()-menu
#'
#' @noRd
#'
#' @importFrom shinydashboard renderMenu sidebarMenu menuItem
#' @importFrom purrr map2

render_sidebar <- function(apptext) {
  # Define sidebar menu content
  menu_items <- data.frame(name = apptext, id = "settings")

  # Build sidebar menu
  sidebarmenu <- shinydashboard::renderMenu(
    shinydashboard::sidebarMenu(
      # Iterate over menu_items name and id
      purrr::map2(
        menu_items$name, menu_items$id,
        \(x, y) shinydashboard::menuItem(x, tabName = y)
      )
    )
  )

  return(sidebarmenu)
}
