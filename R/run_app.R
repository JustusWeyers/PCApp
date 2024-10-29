#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' @param lang name of the language in internal data 'apptext' to use
#' @param webmode enable webmode or not
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(port = 3838),
    enableBookmarking = NULL,
    uiPattern = "/",
    lang = "en",
    webmode = FALSE,
    ...) {
  with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      lang = lang,
      webmode = webmode
    )
  )
}
