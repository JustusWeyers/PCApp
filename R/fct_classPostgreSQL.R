#' classPostgreSQL
#'
#' @description The definition of PostgreSQL class and methods
#'
#' @importFrom RPostgres Postgres
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbCanConnect dbConnect dbGetQuery dbExecute dbWriteTable
#' @importFrom DBI dbReadTable dbAppendTable dbListTables dbRemoveTable
#' @importFrom methods signature is slotNames
#' @importFrom stats na.omit
#' @importFrom purrr map_vec
#'
#' @noRd

# Class definition

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

# Methods

# Connect to a database. First checks if a connection is possible. Returns
# connection or fails to trigger exception handling routine.
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

# Fetch all database users.
setMethod("database.users",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            # SQL query to fetch users
            sql <- r"(SELECT * FROM pg_catalog.pg_user;)"
            # Perform query and return result
            d@users <- DBI::dbGetQuery(d@con, sql)
            return(d@users)
          })

# Fetch all database tables.
setMethod("database.tables",
          methods::signature(d = "PostgreSQL"),
          function(d) {
            # SQL query
            sql <- r"(SELECT * FROM information_schema.tables;)"
            # Perform query and return result
            d@tables <- DBI::dbGetQuery(d@con, sql)
            return(d@tables)
          })

# Fetch database schemas.
setMethod("database.schemas",
          methods::signature(d = "PostgreSQL"),
          function(d, newUser, password) {
            sql = paste0(r"(SELECT nspname FROM pg_catalog.pg_namespace;)")
            d@schemas <- DBI::dbGetQuery(d@con, sql)
            return(d@schemas$nspname)
          })

# Create a new database user. Needs appropriate permission.
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

# Create a new schema for a specific user. Needs appropriate permission.
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

# Fetch the database search path.
setMethod("database.searchpath",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL query
            sql = r"(SHOW search_path;)"
            # Run query and return result
            d@searchpath <- DBI::dbGetQuery(d@con, sql)
            return(d@searchpath$search_path)
          })

# Fetch the tables created by the current user.
setMethod("user.tables",
          methods::signature(d = "PostgreSQL"),
          function(d){
            # SQL query
            sql = r"(SELECT * FROM pg_tables t WHERE t.tableowner = current_user;)"
            # Run query and return result
            d@usertables <- DBI::dbGetQuery(d@con, sql)
            return(d@usertables)
          })

# Write a table to a database and register it in the same step in primary table.
# It returns the key of the written data since it becomes determined by the
# database management system when added to primary table.
setMethod("write.data",
          methods::signature(d = "PostgreSQL"),
          function(d, dataObject, data){
            print("write.data")
            ### PRIMARYTABLE
            # Eventually add entry to primary table
            if (!(dataObject@name %in% get.table(d, "primary_table")$name))
              DBI::dbAppendTable(
                d@con,
                "primary_table",
                value = data.frame(
                  name   = dataObject@name,
                  dtype  = dataObject@dtype,
                  dgroup = dataObject@dgroup
            ))
            # Get dataObject key from primary_table
            dataObject@key <- get.key(d, dataObject)


            ### DATAGROUP TABLE
            # Create temporary object data.frame
            attributedf = s4_to_dataframe(dataObject)
            readparamdf = as.data.frame(dataObject@readparam)
            objectdf = merge(attributedf, readparamdf)

            # Replace row in database by new row
            grouptablename = get.dgroup(d, dataObject)
            grouptable = get.table(d, grouptablename)
            common = intersect(colnames(grouptable), colnames(objectdf))
            if (dataObject@key %in% grouptable$key) {
              grouptable[grouptable$key == dataObject@key,common] = objectdf[,common]
            } else {
              grouptable[nrow(grouptable)+1,common] = objectdf[,common]
            }
            write.dbtable(d, grouptablename, grouptable)

            ### DATA
            # (Over-) write Table
            if (!is.null(data)){
              DBI::dbWriteTable(d@con, name = dataObject@name, value = data, overwrite = TRUE)
            }
            return(dataObject@key)
          })

# This central table contains an overview over the loaded data. Gives every
# table its unique key which serves as primary key.
setMethod("create.primarytable",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL command to create primary table
            sql = r"(
              CREATE TABLE primary_table (
                  key            serial primary key,
                  name           VARCHAR(100) not null,
                  dtype          VARCHAR(100) not null,
                  dgroup         NUMERIC not null
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

# Besides the primary_table one of the two central tables. Contains the used
# data groups and takes care to give every group its unique primary key.
setMethod("create.datagrouptable",
          methods::signature(d = "PostgreSQL"),
          function(d, user){
            # SQL command to create group table
            sql = r"(
              CREATE TABLE datagroup_table (
                  key            serial primary key,
                  name           VARCHAR(100) not null,
                  dtype          VARCHAR(100) not null,
                  color          VARCHAR(100) not null,
                  readmethod     VARCHAR(100) not null
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

# Fetch a database table by name.
setMethod("get.table",
          methods::signature(d = "PostgreSQL"),
          function(d, tablename){
            DBI::dbReadTable(d@con, name = tablename)
          })

# Append a dataframe to an database existing table.
setMethod("appendto.table",
          methods::signature(d = "PostgreSQL"),
          function(d, table, values){
            # Use the DBI dbAppendTable function
            DBI::dbAppendTable(d@con, table, values)
          })

# Delete objects containing data frm database. The delete routine differs for
# different data- or grouptypes. Takes care to delete data as well as the datas
# references in e.g. primary_table or grouptable.
setMethod("delete.data",
          methods::signature(d = "PostgreSQL"),
          function (d, dataObject) {

            # Delete a group
            if(methods::is(dataObject, "Group")) {
              # Delete groups data tables from db
              sql = paste0(r"(SELECT name FROM primary_table WHERE dgroup = ')", dataObject@key, r"(';)")
              lapply(dbGetQuery(d@con, sql)$name, function(tbl) DBI::dbRemoveTable(d@con, tbl))
              # Delete group detail table
              DBI::dbRemoveTable(d@con, dataObject@name)
              # Delete from primary table
              sql = paste0(r"(DELETE FROM primary_table WHERE dgroup = ')", dataObject@key, r"(';)")
              DBI::dbExecute(d@con, sql)
              # Delete from group table
              sql = paste0(r"(DELETE FROM datagroup_table WHERE name = ')", dataObject@name, r"(';)")
              DBI::dbExecute(d@con, sql)
            }

            # Delete a time series
            if (methods::is(dataObject, "Timeseries") | methods::is(dataObject, "Metadata")) {
              # Delete from primary table
              sql = paste0(r"(DELETE FROM primary_table WHERE key = ')", dataObject@key, r"(';)")
              DBI::dbExecute(d@con, sql)
              # Delete from group detail table
              sql = paste0(r"(SELECT * FROM datagroup_table WHERE key = )", dataObject@dgroup, r"(;)")
              groupname = DBI::dbGetQuery(d@con, sql)$name
              sql = paste0(r'(DELETE FROM ")', groupname,r'(" WHERE key = )', dataObject@key, r"(;)")
              DBI::dbExecute(d@con, sql)
              # Delete table
              DBI::dbRemoveTable(d@con, dataObject@name)
            }

          })

# Takes a connection, a table name and a value and writes it to the connected
# database. Any preexisting table with the same name will be overwritten.
setMethod("write.dbtable",
          methods::signature(d = "PostgreSQL"),
          function (d, tablename, value) {
            DBI::dbWriteTable(d@con, tablename, value, overwrite = TRUE)
          })

setMethod("get.dgroup",
          methods::signature(d = "PostgreSQL"),
          function (d, dataObject) {
            sql = paste0(r"(SELECT name FROM datagroup_table WHERE key = ')", dataObject@dgroup, r"(';)")
            g = DBI::dbGetQuery(d@con, sql)$name
            return(g)
          })

setMethod("get.key",
          methods::signature(d = "PostgreSQL"),
          function (d, dataObject) {
            sql = paste0(r"(SELECT key FROM primary_table WHERE name = ')", dataObject@name, r"(';)")
            return(DBI::dbGetQuery(d@con, sql)$key)
          })

setMethod("delete.row",
          methods::signature(d = "PostgreSQL"),
          function (d, table, field, cond) {
            sql = paste0(r'(DELETE FROM ")', table, r'(" WHERE )', field, " = ", cond, ";")
            DBI::dbExecute(d@con, sql)
          })

setMethod("update.table",
          methods::signature(d = "PostgreSQL"),
          function (d, table, field, val, key) {
            if (is(val, "character")) {
              sql = paste0(r'(UPDATE )', table, r'( SET )', field, " = '", val, "' WHERE key = ", key, ";")
            } else {
              sql = paste0(r'(UPDATE )', table, r'( SET )', field, " = ", val, " WHERE key = ", key, ";")
            }
            DBI::dbExecute(d@con, sql)
          })
