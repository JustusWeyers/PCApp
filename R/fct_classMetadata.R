#' classMetadata
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Class definition
source("R/fct_classTableData.R")

setClass("Metadata",
         contains = "TableData"#,
         # slots = c(
         # ),
         # prototype = list(
         # )
)


# Methods

## Read metadata from OS

setMethod("read.data",
          methods::signature(obj = "Metadata"),
          function (obj) {
            if (obj@fileext == "csv") {
              df = read.csv(obj@filepath)
              return(df)
            }

          })
