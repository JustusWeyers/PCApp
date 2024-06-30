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

# if (is.character(c) & !is.na(suppressWarnings(as.numeric(c)))) {
#   # Convert to numeric
#   change[1] <- suppressWarnings(as.numeric(c))
#   return(change)
# } else {
#   return(change)
# }
