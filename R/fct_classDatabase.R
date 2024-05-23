#' classDatabase
#'
#' @description Database class.
#'
#' @noRd

setClass("Database",
         slots = c(
           tables = "data.frame",
           searchpath = "data.frame",
           user = "character"
         ))

# Generics

## Database connection

### Methods to connect a database. In case of the SQLite connetion a local file
### named "sqlite.db" is created inside package folder.
setGeneric("connect.database", function(d) standardGeneric("connect.database"))

## Get database users

### Methods to get information about available database users. Returns nothing
### for SQLite connection since there are no SQLite users.
setGeneric("database.users", function(d) standardGeneric("database.users"))

## Get tables

### Get all the available database tables. Useful for example in login procedure
### as superuser in order to check if there is already a corresponding user
### registrated.
setGeneric("database.tables", function(d) standardGeneric("database.tables"))

## Get schemas

### Method to fetch postgres schemas. Returns nothing for a SQLite connection.
### Useful for investigating postgres seqrchpath.
setGeneric("database.schemas", function(d, newUser, password) standardGeneric("database.schemas"))

## Create user

### Create a new database user. This works only for postgres connection since
### there are no SQLite users. Needs to be executed as database user with
### "create user" permission.
setGeneric("create.user", function(d, newUser, password) standardGeneric("create.user"))

## Create schema

### Dreate a schema with explicit authorisation for certain user. Runs well
### together with create.user routine.
setGeneric("create.schema", function(d, user) standardGeneric("create.schema"))

## Get search path

### Returns postgres searchpath.
setGeneric("database.searchpath", function(d, user) standardGeneric("database.searchpath"))

## Get user tables

### Fetch the tables of the current user. In case of an SQLite connection just
### return the tables available.
setGeneric("user.tables", function(d) standardGeneric("user.tables"))

# Write table

## Write a table.
setGeneric("write.data", function(d, dataObject, data) standardGeneric("write.data"))

## Create primary table

### Special method to create primary table with columns "key", "name" and
### "dtype". Every user has exactly one primary table.
setGeneric("create.primarytable", function(d, user) standardGeneric("create.primarytable"))

## Create table for data groups

### Special method to create data group table with columns "key", "name",
### "dtype" and "color".
setGeneric("create.datagrouptable", function(d, user) standardGeneric("create.datagrouptable"))

## Get table

setGeneric("get.table", function(d, tablename) standardGeneric("get.table"))

## Append row to table

setGeneric("appendto.table", function(d, table, values) standardGeneric("appendto.table"))

## Delete related database entries of an object

setGeneric("delete.data", function(d, dataObject) standardGeneric("delete.data"))

## Create one table for a single data group
setGeneric("create.group_table", function(d, g) standardGeneric("create.group_table"))

## Reimplementation of DBI::dbwritetable
setGeneric("write.dbtable", function(d, tablename, value) standardGeneric("write.dbtable"))

# Apocrypha

# # Delete table
#
# ## Delete a table.
#
# setGeneric("delete.dbtable", function(d, name) standardGeneric("delete.dbtable"))
#
# setMethod("delete.dbtable",
#           methods::signature(d = "PostgreSQL"),
#           function(d, name){
#             # Delete the table from the database
#             DBI::dbRemoveTable(d@con, name)
#             # SQL comand to remove table row from primary table
#             sql = paste0(r"(DELETE FROM primary_table WHERE name = ')", name, r"(';)")
#             DBI::dbExecute(d@con, sql)
#           })
#
# setMethod("delete.dbtable",
#           methods::signature(d = "SQLite"),
#           function(d, name){
#             # Remove table
#             DBI::dbRemoveTable(d@con, name)
#             # SQL comand to remove table row from primary table
#             sql = paste0(r"(DELETE FROM primary_table WHERE name = ')", name, r"(';)")
#             DBI::dbExecute(d@con, sql)
#           })

setGeneric("get.dgroup", function(d, dataObject) standardGeneric("get.dgroup"))
setGeneric("get.key", function(d, dataObject) standardGeneric("get.key"))
setGeneric("delete.row", function(d, table, field, cond) standardGeneric("delete.row"))

