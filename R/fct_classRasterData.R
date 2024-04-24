#' classRasterData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

setClass("RasterData",
         contains = "GeoSpatialData",
         slot = c(
           extent = "character"
         ),
         prototype = list(
           extent = NA_character_
         )
)
