#' database
#'
#' @description Check availability and eventually connect to PostgreSQL DB
#'
#' @param hosts Vector of hostnames to try out
#' @return con DBI::dbConnect()-S4 object or NULL if no DB has been linked
#'
#' @noRd
#'
#' @importFrom DBI dbCanConnect dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom stats setNames

connect_db = function(hosts) {

  # Loop over input hostnames vector
  for (host in hosts) {

    # Default access values
    par <- stats::setNames(c("user", "mysecretpassword", host, "5432", "mydb"),
                    c("user", "password", "host", "port", "dbname"))

    # Check if connection to database is possible
    check <- DBI::dbCanConnect(RPostgres::Postgres(),
        user = getElement(par, "user"),
        password = getElement(par, "password"),
        host = getElement(par, "host"),
        port = getElement(par, "port"),
        dbname = getElement(par, "dbname")
        )

    # If so connect and return con
    if (check == TRUE) {
      con <- DBI::dbConnect(RPostgres::Postgres(),
        user = getElement(par, "user"),
        password = getElement(par, "password"),
        host = getElement(par, "host"),
        port = getElement(par, "port"),
        dbname = getElement(par, "dbname")
      )

      return(con)
    }
  }

  # If no connection has been established return NULL
  return(con = NULL)

}
