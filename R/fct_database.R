#' Classes and methods for an S4 database object
#'
#' @description Classes and methods for S4 database management with sqlite or postgres
#'
#' @return Two possible S4 DBMS objects to choose.
#'
#' @importFrom RPostgres Postgres
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbCanConnect dbConnect dbGetQuery dbExecute dbWriteTable
#' @importFrom DBI dbReadTable dbAppendTable dbDeleteTable dbListTables
#' @importFrom methods signature
#'
#' @noRd

# Database classes

setClass("Database",
         slots = c(
           tables = "data.frame",
           searchpath = "data.frame",
           user = "character"
         ))

## Postgres database class

setClass("PostgreSQL",
         contains = "Database",
         slots = list(
           con = "PqConnection",
           host = "character",
           port = "character",
           password = "character",
           dbname = "character",
           users = "data.frame",
           schemas = "data.frame",
           usertables = "data.frame"
         ), prototype = list(
           host = NA_character_,
           port = NA_character_,
           user = NA_character_,
           password = NA_character_,
           dbname = NA_character_
         ))

## SQLite database class

setClass("SQLite",
         contains = "Database",
         slots = c(
           con = "SQLiteConnection"
         ))


# Generics and Methods

## Database connection

### Methods to connect a database. In case of the SQLite connetion a local file
### named "sqlite.db" is created inside package folder.

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
            # If connection to database is possible connect
            if (con_check == TRUE) {
              d@con <- DBI::dbConnect(
                RPostgres::Postgres(),
                user = d@user,
                password = d@password,
                host = d@host,
                port = d@port,
                dbname = d@dbname
              )
              # Eventually create primary_table
              if (!("primary_table" %in% user.tables(d)$tablename)) {
                create.primarytable(d, user = d@user)
              }
              # Return
              return(d)
            }
            # Important: causes try catch to fail
            stop("Connection to postgres not possible")
          })

setMethod("connect.database",
          methods::signature(d = "SQLite"),
          function (d) {
            # Create local database dir inside package folder.
            # See documentation for create_dir in utils_helpers.R
            extdata_path <- create_dir(
              file.path(system.file(package = "PCApp"), "extdata")
            )
            # Establish a connection to the SQLite file
            d@con <- DBI::dbConnect(
              RSQLite::SQLite(),
              file.path(extdata_path, "sqlite.db")
            )
            # Eventually create primary_table
            if (!("primary_table" %in% user.tables(d)$tablename)) {
              create.primarytable(d, user = NA)
            }
            return(d)
          })

## Get database users

### Methods to get information about available database users. Returns nothing
### for SQLite connection since there are no SQLite users.

setGeneric("database.users", function(d) standardGeneric("database.users"))

setMethod("database.users",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            # SQL query to fetch users
            sql <- r"(SELECT * FROM pg_catalog.pg_user;)"
            # Perform query and return result
            d@users <- DBI::dbGetQuery(d@con, sql)
            return(d@users)
          })

setMethod("database.users",
          methods::signature(d = "SQLite"),
          function(d) {
            # Doing nothing
            return(data.frame())
          })

## Get tables

### Get all the available database tables. Useful for example in login procedure
### as superuser in order to check if there is already a corresponding user
### registrated.

setGeneric("database.tables", function(d) standardGeneric("database.tables"))

setMethod("database.tables",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            # SQL query
            sql <- r"(SELECT * FROM information_schema.tables;)"
            # Perform query and return result
            d@tables <- DBI::dbGetQuery(d@con, sql)
            return(d@tables)
          })

setMethod("database.tables",
          methods::signature(d = "SQLite"),
          function(d) {
            # Run query and return result
            d@tables = data.frame(tablename = DBI::dbListTables(d@con))
            return(d@tables)
          })

## Get schemas

### Method to fetch postgres schemas. Returns nothing for a SQLite connection.
### Useful for investigating postgres seqrchpath.

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
            return(NULL)
          })

## Create user

### Create a new database user. This works only for postgres connection since
### there are no SQLite users. Needs to be executed as database user with create
### user permission.

setGeneric("create.user", function(d, newUser, password) standardGeneric("create.user"))

setMethod("create.user",
          methods::signature(d = "PostgreSQL"),
          function(d, newUser, password) {
            # For the logs
            print("CREATE USER")
            # SQL command
            sql = paste0(r"(CREATE USER )", newUser, r"( WITH PASSWORD ')", password, r"(';)")
            # Execution
            DBI::dbExecute(d@con, sql)
          })

setMethod("create.user",
          methods::signature(d = "SQLite"),
          function(d, newUser, password){
            # Doing nothing
          })

## Create schema

### Dreate a schema with explicit authorisation for certain user. Runs well
### together with create.user routine.

setGeneric("create.schema", function(d, user) standardGeneric("create.schema"))

setMethod("create.schema",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # For the logs
            print("CREATE SCHEMA")
            # SQL command
            sql = paste0(r"(CREATE SCHEMA AUTHORIZATION )",  user, r"(;)")
            # Execution
            DBI::dbExecute(d@con, sql)
          })

setMethod("create.schema",
          methods::signature(d = "SQLite"),
          function(d, user){
            # Doing nothing
          })

## Show search path

### Returns postgres searchpath.

setGeneric("database.searchpath", function(d, user) standardGeneric("database.searchpath"))

setMethod("database.searchpath",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL query
            sql = r"(SHOW search_path;)"
            # Run query and return result
            d@searchpath <- DBI::dbGetQuery(d@con, sql)
            return(d@searchpath$search_path)
          })

setMethod("database.searchpath",
          methods::signature(d = "SQLite"),
          function(d, user){
            # Noing dothing
          })

## Get user tables

### Fetch the tables of the current user. In case of an SQLite connection just
### return the tables available.

setGeneric("user.tables", function(d) standardGeneric("user.tables"))

setMethod("user.tables",
          methods::signature(d = "PostgreSQL"),
          function(d){
            # SQL query
            sql = r"(SELECT * FROM pg_tables t WHERE t.tableowner = current_user;)"
            # Run query and return result
            d@usertables <- DBI::dbGetQuery(d@con, sql)
            return(d@usertables)
          })

setMethod("user.tables",
          methods::signature(d = "SQLite"),
          function(d){
            # Run query and return result
            d@tables = data.frame(tablename = DBI::dbListTables(d@con))
            return(d@tables)
          })

# Write table

## Write a table.

setGeneric("write.dbtable", function(d, name, df, dtype) standardGeneric("write.dbtable"))

setMethod("write.dbtable",
          methods::signature(d = "PostgreSQL"),
          function(d, name, df, dtype){
            # Eventually add entry to primyary table
            if (!(name %in% user.tables(d)$tablename)) {
              DBI::dbAppendTable(d@con, "primary_table",
                                 value =
                                   data.frame(
                                     name = name,
                                     datatype = dtype)
                                 )
            }
            # (Over-) write Table
            DBI::dbWriteTable(d@con, name, df, overwrite = TRUE)
          })

setMethod("write.dbtable",
          methods::signature(d = "SQLite"),
          function(d, name, df, dtype){
            # Eventually add entry to primyary table
            if (!(name %in% d@tables$tablename)) {
              DBI::dbAppendTable(d@con, "primary_table",
                                 value =
                                   data.frame(
                                     name = name,
                                     datatype = dtype)
                                 )
            }
            # (Over-) write table
            DBI::dbWriteTable(d@con, name, df, overwrite = TRUE)
          })

# Delete table

## Delete a table.

setGeneric("delete.dbtable", function(d, name) standardGeneric("delete.dbtable"))

setMethod("delete.dbtable",
          methods::signature(d = "PostgreSQL"),
          function(d, name){
            # Delete the table from the database
            DBI::dbRemoveTable(d@con, name)
            # SQL comand to remove table row from primary table
            sql = paste0(r"(DELETE FROM primary_table WHERE name = ')", name, r"(';)")
            DBI::dbExecute(d@con, sql)
          })

setMethod("delete.dbtable",
          methods::signature(d = "SQLite"),
          function(d, name){
            # Remove table
            DBI::dbRemoveTable(d@con, name)
            # SQL comand to remove table row from primary table
            sql = paste0(r"(DELETE FROM primary_table WHERE name = ')", name, r"(';)")
            DBI::dbExecute(d@con, sql)
          })


## Create primary table

### Special method to create primary table with columns "key", "name" and
### "datatype". Every user has exactly one primary table.

setGeneric("create.primarytable", function(d, user) standardGeneric("create.primarytable"))

setMethod("create.primarytable",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL command
            sql = r"(
              CREATE TABLE primary_table (
                  key            serial primary key,
                  name           VARCHAR(40) not null,
                  datatype       VARCHAR(40) not null
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

setMethod("create.primarytable",
          methods::signature(d = "SQLite"),
          function(d, user){
            # SQL command
            sql = r"(
              CREATE TABLE primary_table (
              key INTEGER PRIMARY KEY  AUTOINCREMENT,
              name           CHAR(40)  NOT NULL,
              datatype       CHAR(40)  NOT NULL
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

# Get table

setGeneric("get.table", function(d, tablename) standardGeneric("get.table"))

setMethod("get.table",
          methods::signature(d = "PostgreSQL"),
          function(d, tablename){
            table = DBI::dbReadTable(d@con, name = tablename)
            return(table)
          })

setMethod("get.table",
          methods::signature(d = "SQLite"),
          function(d, tablename){
            table = DBI::dbReadTable(d@con, name = tablename)
            return(table)
          })

