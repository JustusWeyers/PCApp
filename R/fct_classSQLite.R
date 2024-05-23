#' classSQLite
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom methods signature is
#' @importFrom DBI dbConnect dbListTables dbAppendTable dbWriteTable dbExecute
#' @importFrom DBI dbReadTable dbGetQuery dbRemoveTable
#' @importFrom RSQLite SQLite
#'
#' @noRd

setClass("SQLite",
         contains = "Database",
         slots = c(
           con = "SQLiteConnection"
         ))

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
            # Eventually create primary_table
            if (!("datagroup_table" %in% user.tables(d)$tablename)) {
              create.datagrouptable(d, user = NULL)
            }
            return(d)
          })

setMethod("database.users",
          methods::signature(d = "SQLite"),
          function(d) {
            # Doing nothing
            return(data.frame())
          })

setMethod("database.tables",
          methods::signature(d = "SQLite"),
          function(d) {
            # Run query and return result
            d@tables = data.frame(tablename = DBI::dbListTables(d@con))
            return(d@tables)
          })

setMethod("database.schemas",
          methods::signature(d = "SQLite"),
          function(d, newUser, password){
            return(NULL)
          })

setMethod("create.user",
          methods::signature(d = "SQLite"),
          function(d, newUser, password){
            # Doing nothing
          })

setMethod("create.schema",
          methods::signature(d = "SQLite"),
          function(d, user){
            # Doing nothing
          })

setMethod("database.searchpath",
          methods::signature(d = "SQLite"),
          function(d, user){
            # Doing nothing
          })

setMethod("user.tables",
          methods::signature(d = "SQLite"),
          function(d){
            # Run query and return result
            d@tables = data.frame(tablename = DBI::dbListTables(d@con))
            return(d@tables)
          })

setMethod("write.data",
          methods::signature(d = "SQLite"),
          function(d, dataObject, data){
            # Eventually add entry to primyary table
            if (!(dataObject@name %in% d@tables$tablename)) {
              DBI::dbAppendTable(d@con, "primary_table",
                                 value =
                                   data.frame(
                                     name        = dataObject@name,
                                     dtype       = dataObject@dtype,
                                     dgroup      = dataObject@dgroup
                                   )
              )
            }
            # (Over-) write table
            DBI::dbWriteTable(d@con, name = dataObject@name, value = data, overwrite = TRUE)
          })

setMethod("create.primarytable",
          methods::signature(d = "SQLite"),
          function(d, user){
            # SQL command to create primary table
            sql = r"(
              CREATE TABLE primary_table (
              key INTEGER PRIMARY KEY  AUTOINCREMENT,
              name           CHAR(100)  NOT NULL,
              dtype          CHAR(100)  NOT NULL,
              dgroup         NUMERIC   NOT NULL
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)

          })

setMethod("create.datagrouptable",
          methods::signature(d = "SQLite"),
          function(d, user){
            # SQL command to create group table
            sql = r"(
              CREATE TABLE datagroup_table (
              key INTEGER PRIMARY KEY  AUTOINCREMENT,
              name           CHAR(100)  NOT NULL,
              dtype          CHAR(100)  NOT NULL,
              color          CHAR(100)  NOT NULL,
              readmethod     CHAR(100)  NOT NULL,
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

setMethod("get.table",
          methods::signature(d = "SQLite"),
          function(d, tablename){
            table = DBI::dbReadTable(d@con, name = tablename)
            return(table)
          })

setMethod("appendto.table",
          methods::signature(d = "SQLite"),
          function(d, table, values){
            DBI::dbAppendTable(d@con, table, values)
          })

# Delete objects containing data frm database. The delete routine differs for
# different data- or grouptypes. Takes care to delete data as well as the datas
# references in e.g. primary_table or grouptable.
setMethod("delete.data",
          methods::signature(d = "SQLite"),
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
            if(methods::is(dataObject, "Timeseries")) {
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

# Create a table for a data group containing all the groups details defined by
# the groups datatype. Uses the utils function S4_to_dataframe(). This makes it
# possible to create different detail tables depending on the objects slots.
setMethod("create.group_table",
          methods::signature(d = "SQLite"),
          function (d, g) {
            if (!(g@name %in% user.tables(d)$tablename)) {
              # Convert S4 objects slots to dataframe
              df = S4_to_dataframe(new(g@dtype))
              # Write group detail table
              DBI::dbWriteTable(d@con, g@name, df[0,])
            }
          })

# Takes a connection, a table name and a value and writes it to the connected
# database. Any preexisting table with the same name will be overwritten.
setMethod("write.dbtable",
          methods::signature(d = "SQLite"),
          function (d, tablename, value) {
            DBI::dbWriteTable(d@con, tablename, value, overwrite = TRUE)
          })
