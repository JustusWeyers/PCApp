#' classMetadata
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

setClass("Metadata",
         contains = "Data",
         slots = c(
           dec = "character",
           sep = "character",
           header = "logical",
           quote = "character",
           na.strings = "character",
           nrows = "numeric",
           skip = "numeric",
           comment.char = "character"
         ),
         prototype = list(
           dec = ".",
           sep = ",",
           header = FALSE,
           quote = "\"'",
           na.strings = "NA",
           nrows = -1,
           skip = 0,
           comment.char = "#"
         )
)

# Methods

## Read metadata from OS

setMethod("read.data",
          methods::signature(obj = "Metadata"),
          function (obj) {
            if (obj@fileext == "csv") {
              df = read.csv(obj@filepath)
              obj@nrow <- nrow(df)
              return(df)
            }

          })
