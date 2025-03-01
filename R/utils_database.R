#' insert_timeseries
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

insert_timeseries = function(d, name, ts) {
  name = paste0("ID", name)
  
  # Fetch timeseriestable
  tst <- get.table(d, "timeseries_table")
  
  # Remove eventually old column
  tst <- subset(tst, select = setdiff(colnames(tst), name))
  print("tst:")
  print(head(tst))
  print("ts:")
  print(head(ts))
  colnames(ts) <- c("timestamp", name)
  # Merge in new timeseries
  tst = merge(tst, ts, by = "timestamp", all = TRUE)
  #
  write.dbtable(d, "timeseries_table", tst)
}

#' instantiatePostgreSQL
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

instantiatePostgreSQL = function(cred, superuser = FALSE) {
  print("instantiatePostgreSQL function")
  print(cred)
  if (superuser) {
    print("SUPERUSER")
    pg = methods::new(
      Class = "PostgreSQL",
      host = getElement(object = cred, name = "host"),
      port = getElement(object = cred, name = "port"),
      user = getElement(object = cred, name = "superuser"),
      password = getElement(object = cred, name = "superpassword"),
      dbname = getElement(object = cred, name = "dbname")
    )
    return(pg)
  } else {
    pg = methods::new(
      Class = "PostgreSQL",
      host = getElement(object = cred, name = "host"),
      port = getElement(object = cred, name = "port"),
      user = getElement(object = cred, name = "user"),
      password = getElement(object = cred, name = "password"),
      dbname = getElement(object = cred, name = "dbname")
    )
    return(pg)
  }
}

#' instantiateSQLite
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

instantiateSQLite = function() {
  new("SQLite")
}

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
s4_to_dataframe <- function(s4obj) {
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

