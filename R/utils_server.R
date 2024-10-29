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

#' setdifflist
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

setdifflist = function(Lobs, Lexp) {
  # Fetch change in input parameters
  change = Lobs[purrr::map_vec(names(Lobs), function(x) !identical(Lobs[[x]], Lexp[[x]]))]
  # Return change as correct datatype
  if (length(change) == 1) {
    return(unlist(change))
  } else {
    return(NULL)
  }
}
