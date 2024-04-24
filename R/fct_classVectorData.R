#' classVectorData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

setClass("VectorData",
         contains = "GeoSpatialData",
         slots = c(
           features = "character"
         ),
         prototype = list(
           features = NA_character_
         )
)

