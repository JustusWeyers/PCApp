#' create_dir
#'
#' @description A utils function to create a directory at given path
#'
#' @return return the input path
#'
#' @noRd

create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE)
  }
  return(path)
}
