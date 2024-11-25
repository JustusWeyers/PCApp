#' classTimeseriesTable
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Class definition
source("R/fct_classTableData.R")

setClass("TimeseriesTable",
         contains = "TableData",
         slots = c(
           cc = "list",
           dparam = "list",
           readmethod = "character",
           readmethods = "character",
           rparam = "list",
           id = "character",
           clname = "character"
         ),
         prototype = list(
           cc = list(
             timestamp = "col_select",
             subject = "text_input",
             unit = "text_input",
             value = "col_select",
             missing_val = "text_input",
             dateformat = "text_input"
           ),
           dparam = list(),
           readmethod = c(
             "read.csv"
           ),
           readmethods = c(
             "read.csv",
             "read.csv2",
             "read.table"
           ),
           rparam = list(),
           clname = NA_character_
         )
)

# Methods

## Generate a box UI

setMethod("boxUI",
          methods::signature(obj = "TimeseriesTable"),
          function (obj) {
            ui = function(id = obj@name) {
              ns <- NS(id)
              shiny::tagList(
                
                ####
                
                # The displayed box
                shiny::uiOutput(ns("ui_databox"))
                
                ####
                
              )
            }
            return(ui)
          })

## Generate box Servers

setMethod("boxServer",
          methods::signature(obj = "TimeseriesTable"),
          definition = function(obj, r, group_server) {
            server <- shiny::moduleServer(obj@name, function(input, output, session) {
              ns <- session$ns
              
              
              ####
              
              
              ####
              
            })
            return(server)
          })
            