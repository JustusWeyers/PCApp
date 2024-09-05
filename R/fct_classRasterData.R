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
           extent = "character",
           cc = "list"
         ),
         prototype = list(
           extent = NA_character_
         )
)
