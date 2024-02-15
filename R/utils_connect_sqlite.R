#' utils_connect_sqlite
#'
#' @description Connect to a SQLite database in package external data
#'
#' @return con DBI::dbConnect()-S4 object
#'
#' @noRd
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite

connect_sqlite <- function() {
  # Set path of extdata where sqlite database is going to be stored
  extdata_path <- create_dir(file.path(system.file(package = "PCApp"), "extdata"))
  # Connect sqlite to database
  return(DBI::dbConnect(RSQLite::SQLite(), file.path(extdata_path, "sqlite.db")))
}
