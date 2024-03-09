#' database
#'
#' @description Classes and Methods for S4 database management
#'
#' @return Several things
#'
#' @importFrom RPostgres Postgres
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbCanConnect dbConnect dbGetQuery dbExecute
#' @importFrom methods signature
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
           schemas = "data.frame",
           tables = "data.frame",
           searchpath = "data.frame"
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

setClass("Data",
         slots = c(
           name = "character",
           size = "character",
           path = "character"
         ),
         prototype = list(
           name = NA_character_,
           size = NA_character_,
           path = NA_character_
         ))

setClass("Timeseries",
         contains = "Data",
         slots = c(
           nrow = "character"
         ),
         prototype = list(
           nrow = NA_character_
         )
)

setClass("GeoSpatialData",
         contains = "Data",
         slots = c(
           EPSG = "character"
         ),
         prototype = list(
           EPSG = NA_character_
         )
)

setClass("VectorData",
         contains = "GeoSpatialData",
         slots = c(
           features = "character"
         ),
         prototype = list(
           features = NA_character_
         )
)

setClass("RasterData",
         contains = "GeoSpatialData",
         slot = c(
           extent = "character"
         ),
         prototype = list(
           extent = NA_character_
         )
)

# Generics and Methods

# Database connection

setGeneric("connect.database", function(d) standardGeneric("connect.database"))

setMethod("connect.database",
          methods::signature(d = "PostgreSQL"),
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
          methods::signature(d = "SQLite"),
          function (d) {
            extdata_path <- create_dir(file.path(system.file(package = "PCApp"), "extdata"))
            d@con <- DBI::dbConnect(RSQLite::SQLite(), file.path(extdata_path, "sqlite.db"))
            return(d)
          })

# # Get database information

# Get users

setGeneric("database.users", function(d) standardGeneric("database.users"))

setMethod("database.users",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            sql <- r"(SELECT * FROM pg_catalog.pg_user;)"
            d@users <- DBI::dbGetQuery(d@con, sql)
            return(d@users)
          })

setMethod("database.users",
          methods::signature(d = "SQLite"),
          function(d) {
            return(d@users)
          })

# Get tables

setGeneric("database.tables", function(d) standardGeneric("database.tables"))

setMethod("database.tables",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            sql <- r"(SELECT * FROM information_schema.tables;)"
            d@tables <- DBI::dbGetQuery(d@con, sql)
            return(d@tables)
          })

setMethod("database.tables",
          methods::signature(d = "SQLite"),
          function(d) {
            sql <- r"(SELECT * FROM sqlite_temp_master WHERE type='table';)"
            d@tables <- DBI::dbGetQuery(d@con, sql)
            return(d@tables)
          })

# Get schemas

setGeneric("database.schemas", function(d, newUser, password) standardGeneric("database.schemas"))

setMethod("database.schemas",
          methods::signature(d = "PostgreSQL"),
          function(d, newUser, password) {
            sql = paste0(r"(SELECT nspname FROM pg_catalog.pg_namespace;)")
            d@schemas <- DBI::dbGetQuery(d@con, sql)
            return(d@schemas$nspname)
          })

setMethod("database.schemas",
          methods::signature(d = "SQLite"),
          function(d, newUser, password){

          })

# Create user

setGeneric("create.user", function(d, newUser, password) standardGeneric("create.user"))

setMethod("create.user",
          methods::signature(d = "PostgreSQL"),
          function(d, newUser, password) {
            print("Create User")
            sql = paste0(r"(CREATE USER )", newUser, r"( WITH PASSWORD ')", password, r"(';)")
            DBI::dbExecute(d@con, sql)
          })

setMethod("create.user",
          methods::signature(d = "SQLite"),
          function(d, newUser, password){

          })

# Create schema

setGeneric("create.schema", function(d, user) standardGeneric("create.schema"))

setMethod("create.schema",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            sql = paste0(r"(CREATE SCHEMA AUTHORIZATION )",  user, r"(;)")
            DBI::dbExecute(d@con, sql)
          })

setMethod("create.schema",
          methods::signature(d = "SQLite"),
          function(d, user){

          })

# Show search path

setGeneric("database.searchpath", function(d, user) standardGeneric("database.searchpath"))

setMethod("database.searchpath",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            sql = r"(SHOW search_path;)"
            d@searchpath <- DBI::dbGetQuery(d@con, sql)
            return(d@searchpath$search_path)
          })

setMethod("database.searchpath",
          methods::signature(d = "SQLite"),
          function(d, user){

          })
