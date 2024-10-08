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
           readmethods = "character"
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
           )
         )
)
