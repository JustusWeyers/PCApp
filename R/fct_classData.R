#' classData
#'
#' @description Parent class for all data related classes.
#'
#' @return class 'Data' with specific slots as well as some generic functions
#'
#' @noRd

# Class definition

setClass("Data",
         slots = c(
           key = "numeric",
           dtype = "character",
           dgroup = "numeric",
           name = "character",
           filename = "character",
           displayname = "character",
           filepath = "character",
           filetype = "character",
           filesize = "integer",
           fileext = "character"
         ),
         prototype = list(
           key = NA_integer_,
           dtype = NA_character_,
           dgroup = NA_integer_,
           name = NA_character_,
           filename = NA_character_,
           displayname = NA_character_,
           filepath = NA_character_,
           filetype = NA_character_,
           filesize = NA_integer_,
           fileext = NA_character_
         ))

# Generics

# Every type of data has a UI in form of a shinydashboard box
setGeneric("boxUI", function(obj) standardGeneric("boxUI"))

# Every type of data has a server for the box UI
setGeneric("boxServer", function(obj, r, group_server) standardGeneric("boxServer"))
