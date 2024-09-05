#' autocorrelation
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

acor = function(data, classes, alpha) {
  # Remove NA's from data
  data = na.omit(data)
  # Turn timestamp into numeric
  data[,1] = as.numeric(data[,1])
  # Turn data into unnamed matrix
  data = unname(as.matrix(data))

  # Setup result table
  acf <- as.data.frame(matrix(ncol = 4, nrow = (length(classes)-1)))
  names(acf) <- c("From", "To", "Autocorrelation", "Number.of.instances")
  # Fill column 'From'
  acf$From = classes[-length(classes)]
  # Fill column 'To'
  acf$To = classes[-1]

  # Loop over rows in result data.frame
  for (i in 1:nrow(acf)) {
    # Calculate pairs via Rcpp function
    pairs = pairs_cpp(data = data, FromTo = c(acf[i,1], acf[i,2]))
    # Calculate correlation coefficient
    acf[i,3] = cor(pairs)[1,2]
    # Count number of instances
    acf[i,4] = nrow(pairs)
    # If correlationcoefficient is smaller than alpha end loop
    if (acf[i,3] < alpha) break
  }


  # Return NA-free result table
  return(na.omit(acf))
}
