#' classPostgreSQL
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom RPostgres Postgres
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbCanConnect dbConnect dbGetQuery dbExecute dbWriteTable
#' @importFrom DBI dbReadTable dbAppendTable dbListTables
#' @importFrom methods signature is
#'
#' @noRd

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
              # Eventually create primary_table
              if (!("datagroup_table" %in% user.tables(d)$tablename)) {
                create.datagrouptable(d, user = d@user)
              }
              # Return
              return(d)
            }
            # Important: causes try catch to fail
            stop("Connection to postgres not possible")
          })

setMethod("database.users",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            # SQL query to fetch users
            sql <- r"(SELECT * FROM pg_catalog.pg_user;)"
            # Perform query and return result
            d@users <- DBI::dbGetQuery(d@con, sql)
            return(d@users)
          })

setMethod("database.tables",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            # SQL query
            sql <- r"(SELECT * FROM information_schema.tables;)"
            # Perform query and return result
            d@tables <- DBI::dbGetQuery(d@con, sql)
            return(d@tables)
          })

setMethod("database.schemas",
          methods::signature(d = "PostgreSQL"),
          function(d, newUser, password) {
            sql = paste0(r"(SELECT nspname FROM pg_catalog.pg_namespace;)")
            d@schemas <- DBI::dbGetQuery(d@con, sql)
            return(d@schemas$nspname)
          })

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

setMethod("database.searchpath",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL query
            sql = r"(SHOW search_path;)"
            # Run query and return result
            d@searchpath <- DBI::dbGetQuery(d@con, sql)
            return(d@searchpath$search_path)
          })

setMethod("user.tables",
          methods::signature(d = "PostgreSQL"),
          function(d){
            # SQL query
            sql = r"(SELECT * FROM pg_tables t WHERE t.tableowner = current_user;)"
            # Run query and return result
            d@usertables <- DBI::dbGetQuery(d@con, sql)
            return(d@usertables)
          })

setMethod("write.data",
          methods::signature(d = "PostgreSQL"),
          function(d, dataObject, data){
            # Eventually add entry to primary table
            if (!(dataObject@name %in% user.tables(d)$tablename)) {
              DBI::dbAppendTable(d@con, "primary_table",
                                 value =
                                   data.frame(
                                     name        = dataObject@name,
                                     dtype       = dataObject@dtype,
                                     dgroup      = dataObject@dgroup
                                   )
              )
            }
            # (Over-) write Table
            DBI::dbWriteTable(d@con, name = dataObject@name, value = data, overwrite = TRUE)
          })

setMethod("create.primarytable",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL command to create primary table
            sql = r"(
              CREATE TABLE primary_table (
                  key            serial primary key,
                  name           VARCHAR(40) not null,
                  dtype          VARCHAR(40) not null,
                  dgroup         NUMERIC not null
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

setMethod("create.datagrouptable",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL command to create group table
            sql = r"(
              CREATE TABLE datagroup_table (
                  key            serial primary key,
                  name           VARCHAR(40) not null,
                  dtype          VARCHAR(40) not null,
                  color          VARCHAR(40) not null
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

setMethod("get.table",
          methods::signature(d = "PostgreSQL"),
          function(d, tablename){
            table = DBI::dbReadTable(d@con, name = tablename)
            return(table)
          })

setMethod("appendto.table",
          methods::signature(d = "PostgreSQL"),
          function(d, table, values){
            DBI::dbAppendTable(d@con, table, values)
          })

setMethod("delete.data",
          methods::signature(d = "PostgreSQL"),
          function (d, dataObject) {
            if(methods::is(dataObject, "Group")) {
              # Delete group tables from db
              sql = paste0(r"(SELECT name FROM primary_table WHERE dgroup = ')", dataObject@key, r"(';)")
              deletetables <- lapply(dbGetQuery(d@con, sql)$name, function(tbl) DBI::dbRemoveTable(d@con, tbl))
              # Delete from primary table
              sql = paste0(r"(DELETE FROM primary_table WHERE dgroup = ')", dataObject@key, r"(';)")
              DBI::dbExecute(d@con, sql)
              # Delete from group table
              sql = paste0(r"(DELETE FROM datagroup_table WHERE name = ')", dataObject@name, r"(';)")
              DBI::dbExecute(d@con, sql)
            }
          })
