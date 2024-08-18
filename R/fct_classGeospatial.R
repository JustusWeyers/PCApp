#' classGeospatial
#'
#' This class inherits from class Data.
#'
#' @description Definition of a data class for geospatial objects.
#'
#' @return An S4 class
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
