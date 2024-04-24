#' classData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
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

setGeneric("read.data", function(obj) standardGeneric("read.data"))

setGeneric("boxUI", function(obj) standardGeneric("boxUI"))

setGeneric("boxServer", function(obj, r, group_data, txt) standardGeneric("boxServer"))

