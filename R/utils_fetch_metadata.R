#' fetch_metadata
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

fetch_metadata = function(metadata, ids, field) {
  if (!is.null(metadata)) {
    do.call(rbind.data.frame, lapply(metadata, function(df) {
      if (all(c("id", field) %in% colnames(df))) {
        return(df[df$id %in% ids, c("id", field)])
      }
    }))
  }
}
