#' fct_connect_database
#'
#' @description Master function in the matter of connecting to a specific databyse type specified in type parameter
#'
#' @param type Prefered databasetype. "PostgreSQL" or "SQLite".
#' @param host host name
#' @param port database port
#' @param user username of database user
#' @param password password for database user
#' @param db database name
#'
#' @return con DBI::dbConnect()-S4 object
#'
#' @noRd

connect_database <- function(type, host = NA, user = "user",
                             password = "mysecretpassword",
                             port = "5432", dbname = "mydb") {

  # The futur connection object
  con = NA

  # Eventually try Postgres connection
  if (type == "PostgreSQL") {
    con <- connect_postgres(
      host = host, port = port, user = user,
      dbname = dbname, password = password
    )
  }

  # Eventually try sqlite connetion
  if (type == "SQLite" | is.na(con)) {
    con <- connect_sqlite(
      host = host, port = port, user = user,
      dbname = dbname, password = password
    )
  }

  return(con)
}
