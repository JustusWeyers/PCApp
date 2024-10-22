#' acor_visualisation
#' 
#' A function to show number of timeseries above certain acor level.
#'
#' @return A plot.
#' 
#' @export
#'

acor_visualisation <- function(accordf, step = 0.05){
  # Subset to extract acor values
  acf_values = accordf[,5:ncol(acordf)]
  
  # Define acor levels to calculate
  acor_level = seq(0.1, 1, by = step)
  
  # Count number of timeseries above level alpha
  n = unlist(lapply(acor_level, function(alpha) {
    df = apply(X = acf_values, MARGIN = 1, FUN = function(r) r > alpha)
    sum(na.omit(apply(df, 2, any)))
  }))
  
  # Prepare df
  plotdf = data.frame(
    acor_level = acor_level, 
    n = n, 
    p = n/nrow(acf)
  )
  
  # Making a plot
  p = ggplot2::ggplot(plotdf, ggplot2::aes(x = acor_level, y = p)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(label = n, vjust = -0.5, hjust = -0.1) +
    ggplot2::scale_x_continuous(
      breaks = seq(0.1, 1, by = 0.2), 
      minor_breaks = seq(0.1, 1, by = 0.1)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, by = 0.2), 
      minor_breaks = seq(0, 1, by = 0.1)
    ) +
    
    ggplot2::theme_minimal()
  
  # Return the plot
  return(p)
}