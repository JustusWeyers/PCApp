#' random_address
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

random_address = function() {
  paste0(sample(letters, 1), sample(c(letters, 0:9), 9), collapse = "")
}
