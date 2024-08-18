#' datareading
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

mydata = function(d, name, readm, g) {
  # Reduce to function parameter
  g = g[names(g) %in% names(optional_fun_param(readm))]
  # Some NA handling
  g[names(g) != "na.strings" & g == "NA"] <- NA
  # Fill argument to read incomplete lines
  g[["fill"]] <- TRUE
  # Request text from database
  g[["text"]] <- get.table(d, name)$data

  tryCatch(expr = {
    return(do.call(readm, g))
  }, error = function(e) {
    return(data.frame())
  })
}

head_data <- function(d, name, readm, g) {
  l = get.table(d, name)$data
  # The headlines to come
  hlines = c()
  # Add headlines based on 'skip'
  if ("skip" %in% names(g)) {
    skip = getElement(g, "skip")
    hlines = c(hlines, 1:skip)
  }
  # Add headlines based on 'comment.char'
  if ("comment.char" %in% names(g)) {
    c.char = getElement(g, "comment.char")
    hlines = c(hlines, which(startsWith(l, c.char)))
  }
  # Filter for hlines
  hlines = l[unique(hlines)]
  # Clean up messy characters
  hlines = sapply(hlines, stringi::stri_trans_general, id = "Latin-ASCII")
  # Clean up messy characters
  return(data.frame(data = hlines))
}
