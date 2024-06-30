#' classTableData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Class definition
source("R/fct_classData.R")

setClass("TableData",
         contains = "Data",
         slots = c(
           head = "character",
           readmethod = "character",
           readmethods = "character",
           param = "list"
         ),
         prototype = list(
           head = "",
           readmethod = "read.table",
           readmethods = c(
             "read.csv",
             "read.csv2",
             "read.delim",
             "read.delim2",
             "read.table",
             "readRDS"
           ),
           param = list()
         )
)



setGeneric("get_data", function(d, obj) standardGeneric("get_data"))

setGeneric("get_cols", function(d, obj) standardGeneric("get_cols"))

setGeneric("get_head_data", function(d, obj) standardGeneric("get_head_data"))
