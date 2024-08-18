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

## Write tabledata to db

### Function adds entry to primary_table, checks which key as been assigned,
### writes tabledata to db. Returns the assigned key.
setGeneric("write.data", function(d, dataObject, data) standardGeneric("write.data"))

## Create primary table

### Special method to create primary_table with columns "key", "name", "dtype"
### (data type) and "dgroup" (data group). Every user has exactly one
### primary_table.
setGeneric("create.primarytable", function(d) standardGeneric("create.primarytable"))

## Create table for data groups

### Method to create datagroup_table with columns "key", "name",
### "dtype", "color", "readmethod" and "param".
setGeneric("create.datagrouptable", function(d) standardGeneric("create.datagrouptable"))

setGeneric("create.timeseriestable", function(d) standardGeneric("create.timeseriestable"))

## Get table

### Fetch a table by name
setGeneric("get.table", function(d, tablename) standardGeneric("get.table"))

## Append row to table

### Add a row to a database table
setGeneric("appendto.table", function(d, table, values) standardGeneric("appendto.table"))

## Delete related database entries of an object

### Delete data objects. Uses specific procedures depending on datatype
setGeneric("delete.data", function(d, dataObject) standardGeneric("delete.data"))

## Write a table to database

### Reimplementation of DBI::dbwritetable.
setGeneric("write.dbtable", function(d, tablename, value) standardGeneric("write.dbtable"))

## Fetch datagroup of a data object

### Every data object has a datagroup. The groupname will be returned.
setGeneric("get.dgroup", function(d, dataObject) standardGeneric("get.dgroup"))

## Fetch key of a data object

### Every data entry in primary_table has an auto-incrementing key (primary key).
### This key is going to be returned. The name of the data object is used to
### identify the corresponding key in primary_table
setGeneric("get.key", function(d, dataObject) standardGeneric("get.key"))

## Delete a row in table by condition

### Deletes the row in a database table where the specified field matches the
### given condition.
setGeneric("delete.row", function(d, table, field, cond) standardGeneric("delete.row"))

## Change a a value in table by field and key

### Takes a table name, the key and field to identify the cell and the replace value as
### arguments
setGeneric("change.tablevalue", function(d, table, key, field, val) standardGeneric("change.tablevalue"))

setGeneric("replace.by.primary_key", function(d, table, key, values) standardGeneric("replace.by.primary_key"))

setGeneric("clear.db", function(d) standardGeneric("clear.db"))

setGeneric("merge.timeseries", function(d, names) standardGeneric("merge.timeseries"))
