#' join_timeseries_with_metadata
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# This function definitely needs some clean up. But it works.

join_timeseries_with_metadata = function(db) {

  print("# --- JOIN TIMESERIES AND METADATA --------")
  user_tables = user.tables(db)$tablename

  primary_table = get.table(db, "primary_table")

  timeseries = primary_table[primary_table$dtype == "Timeseries", "name"]

  metadata = primary_table[primary_table$dtype == "Metadata", "name"]
  metadata = metadata[sapply(metadata, function(n) paste0(n, "_clean") %in% user_tables)]

  if (length(metadata)>0) {
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

    matched_ids = unlist(matched_ids)
    matched_ids = matched_ids[!is.null(unname(matched_ids))]

    if (is.null(matched_ids)) {
      return(NULL)
    }

    matched_ids = trimws(format(matched_ids, scientific = FALSE))
    matched_ids = data.frame(name = names(matched_ids), id = unname(matched_ids))
    primary_table = primary_table[,colnames(primary_table) != "id"]
    primary_table = merge(primary_table, matched_ids, by = "name", all.x = TRUE)

    lapply(primary_table$key, function(key) {
      change.tablevalue(db, "primary_table", key, "id", primary_table[primary_table$key == key,"id"])
    })

    colnames(matched_ids) = c("hash", "id")

    # Create a list of metadata for the matched ids
    metadata_for_global = lapply(metadata, function(n) {
      md = get.table(db, paste0(n, "_clean"))
      if ("id" %in% colnames(md)) {
        md$id = trimws(format(md$id, scientific = FALSE))
        md = merge(matched_ids, md, by = "id", all.x = TRUE)
        print(md)
        return(md)
      } else {
        return(NULL)
      }
    })

    print("r metadada")
    print(head(metadata_for_global))

    # Return the list in orer to store it in global variable r
    return(metadata_for_global)

  }

}
