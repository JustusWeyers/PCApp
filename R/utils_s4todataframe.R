#' s4todataframe
#'
#' @description This function turns an S4 objects slots and returns a dataframe
#'
#' @return Dataframe with slotnames as colnames and slot values as rows
#'
#' @importFrom methods slotNames
#' @importFrom stats na.omit
#' @importFrom purrr map_vec
#'
#' @noRd

# Create one column for every slot
S4_to_dataframe <- function(s4obj) {
  # Get names of slots
  nms <- methods::slotNames(s4obj)
  # Slot class types compatible with database table
  ok = c("character", "integer", "numeric", "logical", "double")
  # Remove slots that do not fit into a dataframe
  nms <- stats::na.omit(purrr::map_vec(nms, function(n) {
    if (typeof(slot(s4obj, n)) %in%  ok & length(slot(s4obj, n)) == 1)
      return(n)
    else
      return(NA)
  }))
  # Create and return dataframe
  slotlist = lapply(nms, function(nm) slot(s4obj, nm))
  names(slotlist) = nms
  return(as.data.frame(slotlist))
}
