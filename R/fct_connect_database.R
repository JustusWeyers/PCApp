#' fct_connect_database
#'
#' @description Check availability and correspondingly connect to PostgreSQL/sqlite DB
#'
#' @param host host name of PostgreSQL database to connect.
#' @param port database port
#' @param user username of database user
#' @param password password of database user
#' @param db database name
#'
#' @return con DBI::dbConnect()-S4 object
#'
#' @noRd
#'
#' @importFrom DBI dbCanConnect dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom RSQLite SQLite

connect_db = function(host = NA, user = "user", password  = "mysecretpassword",
                      port = "5432", dbname = "mydb") {

  print(host)

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

  # Set path of extdata
  extdata_path = create_dir(file.path(system.file(package = "PCApp"), "extdata"))
  # If no db has been connected return sqlite con
  return(DBI::dbConnect(RSQLite::SQLite(), file.path(extdata_path, "db.sqlite")))
}
