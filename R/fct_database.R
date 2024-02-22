#' database
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom RPostgres Postgres
#' @importFrom RSQLite SQLite
#'
#' @noRd

# Classes

setClass("Database",
         slots = c(
           host = "character",
           port = "character",
           user = "character",
           password = "character",
           dbname = "character",

           users = "data.frame",
           tables = "data.frame"
         ),
         prototype = list(
           host = NA_character_,
           port = NA_character_,
           user = NA_character_,
           password = NA_character_,
           dbname = NA_character_
         ))

setClass("PostgreSQL",
         contains = "Database",
         slots = list(
           con = "PqConnection"
         ))


setClass("SQLite",
         contains = "Database",
         slots = c(
           con = "SQLiteConnection"
         ))

# Generics

# Database connection

setGeneric("connect.database", function(d) standardGeneric("connect.database"))

setMethod("connect.database",
          signature(d = "PostgreSQL"),
          function (d) {
            # Check if connection to database is possible
            con_check <- DBI::dbCanConnect(
              RPostgres::Postgres(),
              user = d@user,
              password = d@password,
              host = d@host,
              port = d@port,
              dbname = d@dbname
            )

            # If connection to database is possible connect and return con
            if (con_check == TRUE) {
              d@con <- DBI::dbConnect(
                RPostgres::Postgres(),
                user = d@user,
                password = d@password,
                host = d@host,
                port = d@port,
                dbname = d@dbname
              )
              return(d)
            }

            stop("Connection to postgres not possible")
          })

setMethod("connect.database",
          signature(d = "SQLite"),
          function (d) {
            extdata_path <- create_dir(file.path(system.file(package = "PCApp"), "extdata"))
            d@con <- DBI::dbConnect(RSQLite::SQLite(), file.path(extdata_path, "sqlite.db"))
            return(d)
          })

# # Get database information

# Get users

setGeneric("database.users", function(d) standardGeneric("database.users"))

setMethod("database.users",
          signature(d = "PostgreSQL"),
          function(d) {
            sql <- r"(SELECT * FROM pg_catalog.pg_user;)"
            d@users <- DBI::dbGetQuery(d@con, sql)
            return(d@users)
          })

setMethod("database.users",
          signature(d = "SQLite"),
          function(d) {
            return(d@users)
          })

# Get tables

setGeneric("database.tables", function(d) standardGeneric("database.tables"))

setMethod("database.tables",
          signature(d = "PostgreSQL"),
          function(d) {
            sql <- r"(SELECT * FROM information_schema.tables WHERE table_schema NOT IN ('pg_catalog', 'information_schema') AND table_schema NOT LIKE 'pg_toast%')"
            d@tables <- DBI::dbGetQuery(d@con, sql)
            return(d@tables)
          })

setMethod("database.tables",
          signature(d = "SQLite"),
          function(d) {
            sql <- r"(SELECT * FROM sqlite_temp_master WHERE type='table';)"
            d@tables <- DBI::dbGetQuery(d@con, sql)
            return(d@tables)
          })


# Create user

setGeneric("create.user", function(d, newUser, password) standardGeneric("create.user"))

setMethod("create.user",
          signature(d = "PostgreSQL"),
          function(d, newUser, password) {
            print("Create User")
            sql = paste0(r"(CREATE USER )", newUser, r"( WITH PASSWORD ')", password, r"(';)")
            DBI::dbExecute(d@con, sql)
          }
)

setMethod("create.user",
          signature(d = "SQLite"),
          function(d, newUser, password){}
          )
