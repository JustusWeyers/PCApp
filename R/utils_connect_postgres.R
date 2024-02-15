#' utils_connect_postgres
#'
#' @description Check availability and correspondingly connect to PostgreSQL/sqlite DB
#'
#' @param host host name of PostgreSQL database to connect.
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

connect_postgres <- function(host, user, password, port, dbname) {
  # Check if connection to database is possible
  con_check <- DBI::dbCanConnect(RPostgres::Postgres(),
    user = user,
    password = password,
    host = host,
    port = port,
    dbname = dbname
  )

  # If connection to database is possible connect and return con
  if (con_check == TRUE) {
    con <- DBI::dbConnect(RPostgres::Postgres(),
      user = user,
      password = password,
      host = host,
      port = port,
      dbname = dbname
    )

    # Return connection object
    return(con)
  }
  # Otherwise return NA
  else {
    return(NA)
  }
}
