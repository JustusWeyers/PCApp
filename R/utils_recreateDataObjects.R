#' recreateDataObjects
#'
#' @description Build data objects from entries in datagroup table
#'
#' @importFrom methods slot
#'
#' @return Object s of class data
#'
#' @noRd

recreateDataObjects = function(...) {
  # Build list from ananymous arguments
  entry = data.frame(...)
  # Select future object attributes
  att = entry[!(colnames(entry) %in% names(formals(entry$readmethod)))]
  # Select objects readparameters
  opt = entry[colnames(entry) %in% names(formals(entry$readmethod))]
  # Instantiate object with readparameters
  newobject = methods::new(entry$dtype, readparam = opt)
  # Assign slot values
  # lapply(colnames(att), function(n) slot(newobject, n) = att[1,n])

  for (cn in colnames(att)) {
    methods::slot(newobject, cn) = att[,cn]
  }

  return(newobject)
}
