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
              file.path(extdata_path, "sqlitedb.db")
            )
            # Eventually create primary_table
            if (!("primary_table" %in% user.tables(d)$tablename)) {
              create.primarytable(d)
            }
            # Eventually create primary_table
            if (!("datagroup_table" %in% user.tables(d)$tablename)) {
              create.datagrouptable(d)
            }
            # Eventually create timeseries_table
            if (!("timeseries_table" %in% user.tables(d)$tablename)) {
              create.timeseriestable(d)
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

setMethod("create.primarytable",
          methods::signature(d = "SQLite"),
          function(d){
            # SQL command to create primary table
            sql = r"(
              CREATE TABLE primary_table (
              key INTEGER PRIMARY KEY  AUTOINCREMENT,
              name           CHAR(100)  NOT NULL,
              dtype          CHAR(100)  NOT NULL,
              dgroup         NUMERIC   NOT NULL,
              rparam         CHAR(9999),
              dparam         CHAR(9999),
              id             CHAR(100)
              );
            )"

            # head           CHAR(9999),
            # clearname      CHAR(100),
            # lat            CHAR(100),
            # lon            CHAR(100)

            # Run command on database
            DBI::dbExecute(d@con, sql)

          })

setMethod("create.datagrouptable",
          methods::signature(d = "SQLite"),
          function(d){
            # SQL command to create group table
            sql =  r"(
              CREATE TABLE datagroup_table (
              key            INTEGER PRIMARY KEY  AUTOINCREMENT,
              name           CHAR(100)  NOT NULL,
              dtype          CHAR(100)  NOT NULL,
              gparam         CHAR(999)
              );
            )"
            # Run command on database
            DBI::dbExecute(d@con, sql)
          })

setMethod("create.timeseriestable",
          methods::signature(d = "SQLite"),
          function(d){
            # SQL command to create group table
            sql = r"(
              CREATE TABLE timeseries_table (
                  timestamp      date
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
              # Delete table
              DBI::dbRemoveTable(d@con, dataObject@name)
            }
          })

# Create a table for a data group containing all the groups details defined by
# the groups datatype. Uses the utils function S4_to_dataframe(). This makes it
# possible to create different detail tables depending on the objects slots.
# setMethod("create.group_table",
#           methods::signature(d = "SQLite"),
#           function (d, g) {
#             if (!(g@name %in% user.tables(d)$tablename)) {
#               # Convert S4 objects slots to dataframe
#               df = S4_to_dataframe(new(g@dtype))
#               # Write group detail table
#               DBI::dbWriteTable(d@con, g@name, df[0,])
#             }
#           })

# Takes a connection, a table name and a value and writes it to the connected
# database. Any preexisting table with the same name will be overwritten.
setMethod("write.dbtable",
          methods::signature(d = "SQLite"),
          function (d, tablename, value) {
            DBI::dbWriteTable(d@con, tablename, value, overwrite = TRUE)
          })

setMethod("get.dgroup",
          methods::signature(d = "SQLite"),
          function (d, dataObject) {
            sql = paste0(r"(SELECT name FROM datagroup_table WHERE key = ')", dataObject@dgroup, r"(';)")
            g = DBI::dbGetQuery(d@con, sql)$name
            return(g)
          })

setMethod("get.key",
          methods::signature(d = "SQLite"),
          function (d, dataObject) {
            sql = paste0(r"(SELECT key FROM primary_table WHERE name LIKE ')", dataObject@name, r"(';)")
            return(DBI::dbGetQuery(d@con, sql)$key)
          })

setMethod("delete.row",
          methods::signature(d = "SQLite"),
          function (d, table, field, cond) {
            sql = paste0(r'(DELETE FROM ")', table, r'(" WHERE )', field, " = ", cond, ";")
            DBI::dbExecute(d@con, sql)
          })

setMethod("change.tablevalue",
          methods::signature(d = "SQLite"),
          function (d, table, key, field, val) {
            if (is(val, "character")) {
              sql = paste0(r'(UPDATE )', table, r'( SET )', field, " = '", val, "' WHERE key = ", key, ";")
            } else {
              sql = paste0(r'(UPDATE )', table, r'( SET )', field, " = ", val, " WHERE key = ", key, ";")
            }
            DBI::dbExecute(d@con, sql)
          })

setMethod("replace.by.primary_key",
          methods::signature(d = "SQLite"),
          function (d, table, key, values) {
            lapply(colnames(values), function(n) {
              change.tablevalue(d, table, key, field = n, val = values[1,n])
            })
          })

setMethod("clear.db",
          methods::signature(d = "SQLite"),
          function (d) {
            #DROP TABLE [IF EXISTS] [schema_name.]table_name;
            tnms = user.tables(d)$tablename
            tnms = setdiff(tnms, c("sqlite_sequence"))
            for (tn in tnms) {
              sql = paste0('DROP TABLE IF EXISTS ', paste0('"', tn, '"'), ';')
              print(sql)
              DBI::dbExecute(d@con, sql)
            }
          })

setMethod("merge.timeseries",
          methods::signature(d = "SQLite"),
          function (d, names) {

            print("merge")

            cnms = colnames(get.table(d, "timeseries_table"))

            # Clean table from old columns
            for (nm in names) {
              if (nm %in% cnms) {
                sql = paste0("ALTER TABLE timeseries_table DROP COLUMN ", nm,";")
                print(sql)
                suppressWarnings(DBI::dbExecute(d@con, sql))
              }
            }

            sql = paste0("with cte as (SELECT * FROM timeseries_table ", paste(paste0("FULL JOIN ", paste0(names, "_clean"), " USING (timestamp)"), collapse = " "), ") select * from cte;")
            df = DBI::dbGetQuery(d@con, sql)
            df = df[order(df$timestamp),]
            df$timestamp = as.Date(df$timestamp)
            df = df[is(df$timestamp, "Date"),]
            print(head(df))

            write.dbtable(d, "timeseries_table", df)
            print("done")
          })
