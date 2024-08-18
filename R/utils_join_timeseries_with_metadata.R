#' join_timeseries_with_metadata
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

join_timeseries_with_metadata = function(db) {

  print("# --- JOIN TIMESERIES AND METADATA --------")
  user_tables = user.tables(db)$tablename

  primary_table = get.table(db, "primary_table")

  timeseries = primary_table[primary_table$dtype == "Timeseries", "name"]
  metadata = primary_table[primary_table$dtype == "Metadata", "name"]

  ids = unique(unlist(lapply(metadata, function(md) {
    get.table(db, paste0(md, "_clean"))$id
  })))

  matched_ids = sapply(timeseries, function(ts) {
    if (paste0(ts, "_head") %in% user_tables) {
      head = toString(get.table(db, paste0(ts, "_head"))$data)
    } else {
      print("No head")
      return(NULL)
    }
    matches = sapply(ids, grepl, x = head)
    if (any(matches)) {
      return(ids[matches][1])
    } else {
      return(NULL)
    }
  })

  matched_ids = na.omit(matched_ids)

  primary_table[primary_table$name %in% names(matched_ids),"id"] <- matched_ids

  write.dbtable(db, "primary_table", primary_table)

}
