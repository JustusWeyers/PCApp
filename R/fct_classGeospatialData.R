#' classGeospatialData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

setClass("GeoSpatialData",
         contains = "Data",
         slots = c(
           EPSG = "character"
         ),
         prototype = list(
           EPSG = NA_character_
         )
)
