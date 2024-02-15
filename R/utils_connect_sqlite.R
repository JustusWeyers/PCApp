#' utils_connect_sqlite
#'
#' @description A description
#'
#' @param host host name
#' @param port database port
#' @param user username of database user
#' @param password password for database user
#' @param db database name
#'
#' @return con DBI::dbConnect()-S4 object
#'
#' @noRd
#'
#' @importFrom DBI dbCanConnect dbConnect
#' @importFrom RSQLite SQLite

connect_sqlite <- function(host = NA, user = "user", password = "mysecretpassword",
                           port = "5432", dbname = "mydb") {
  # Set path of extdata where sqlite database is going to be stored
  extdata_path <- create_dir(file.path(system.file(package = "PCApp"), "extdata"))
  # Connect sqlite to database
  print(file.path(extdata_path, "db.sqlite"))
  return(DBI::dbConnect(RSQLite::SQLite(), file.path(extdata_path, "db.sqlite")))
}
