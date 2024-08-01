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


# --- Test ---------

# # Generate random timeseries data.frame
# ts = function() {
#   dates = seq(as.Date("1960-01-01"), as.Date("1960-12-31"), by = "days")
#   tst = data.frame(timestamp = dates, value = rnorm(length(dates)))
#   tst = tst[sort(sample.int(length(dates), 100)),]
# }
#
# ts1 = ts()
# ts2 = ts()
#
# # Merge the two timeseries
# tst = merge(ts1, ts2, by = "timestamp", all = TRUE)
