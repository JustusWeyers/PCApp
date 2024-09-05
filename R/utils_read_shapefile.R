#' read_shapefile
#'
#' @description Read a shape file from temp directory
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
read_shapefile = function(shp_path) {
  if(!is.null(shp_path)){
    # Get path of the temp directory
    temp = dirname(shp_path)
    # List all temp files
    files = list.files(temp, full.names = TRUE)
    # Rename all files by replacing filename by 'x'
    renamed = sapply(files, function(p) {
      bn = basename(p)
      fex = tools::file_ext(p)
      return(gsub(bn, paste0("x.", fex), p))
    })
    file.rename(files, renamed)
    # Change name of the original shp_path to 'x' as well
    shp_path = gsub(basename(shp_path), "x.shp", shp_path)
    # Read and return shape file as simple feature
    return(sf::st_read(shp_path, quiet = TRUE))
  }
}
