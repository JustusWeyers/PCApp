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
    c = unlist(change)
    if (is.character(c) & !is.na(as.numeric(c))) return(change[[1]] <- as.numeric(c))
    else return(change)
  } else return(NULL)
}
