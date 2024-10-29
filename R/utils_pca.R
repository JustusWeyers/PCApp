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

#' format_plot 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

format_plot = function(p) {
  cap = paste("Source: PCApp", format(Sys.Date(), "%Y"))
                  
  if (is.null(p)) return(NULL)
  p = p +
    ggplot2::labs(caption = cap) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(color = "gray", face = "italic"),
      plot.caption.position = "plot",
      plot.background = ggplot2::element_rect(colour = "white")
    )
  return(p)
}

#' plots 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' 
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual element_blank ylab
#' @importFrom ggplot2 ggtitle  coord_flip theme_minimal theme element_text 
#'
#' @noRd

avaliable_ts = function(df1, df2, txt) {
  ggplot2::ggplot() +
    # Create bar plot
    ggplot2::geom_bar(
      data=df1, 
      ggplot2::aes(x = group, y = occurences),
      fill = df1$color,
      stat="identity") +
    # Create bar plot
    ggplot2::geom_bar(
      data = df2, 
      ggplot2::aes(x = group, y = occurences),
      fill = df2$color,
      stat="identity") +
    # Fill bars with group colors
    ggplot2::scale_fill_manual(values = rev(df1$color)) +
    # Add A Title
    ggplot2::ggtitle(txt[[82]]) +
    # Add a
    ggplot2::ylab(txt[[83]]) +
    # Make bar chart horizontal
    ggplot2::coord_flip() +
    # Set theme minimal
    ggplot2::theme_minimal() +
    # Some more settings regarding the theme
    ggplot2::theme(
      # Make title font bold
      plot.title = ggplot2::element_text(face="bold", size = 15),
      # Remove y axis title
      axis.title.y = ggplot2::element_blank(),
      # Place title on left margin
      plot.title.position = "plot",
      # Remove legend
      legend.position = "none"
    )
}

# alpha_plot = function(acf, input_alpha) {
#   
#   if (is.null(acf) | is.null(input_alpha)) return(NULL)
#   
#   alphas = seq(0.1, 1.0, by = 0.1)
#   if (!(input_alpha %in% alphas)) {
#     alphas = c(input_alpha, alphas)
#   }
#   
#   v = purrr::map_df(alphas, function(a) {
#     df = acf[,3:ncol(acf)] > a
#     alpha_text = paste0(" >", a)
#     any_trues = apply(X = df, MARGIN = 1, FUN = any)
#     return(setNames(sum(any_trues, na.rm = TRUE), alpha_text))
#   })
#   
#   plotdf = as.data.frame(colSums(v, na.rm = TRUE), nm = "n")
#   
#   print(plotdf)
#   
#   plotdf$alpha_txt = rownames(plotdf)
#   plotdf$alpha = alphas
#   plotdf$p = plotdf$n/nrow(acf) * 100
#   plotdf$color = (plotdf$alpha == input_alpha)
#   
#   p = ggplot2::ggplot(plotdf, ggplot2::aes(x = alpha, y = p, group=1, label = n)) +
#     ggplot2::geom_point(ggplot2::aes(color=color)) +
#     ggplot2::scale_color_manual(values = c("black", "red")) +
#     ggplot2::geom_text(vjust = -0.3, hjust = -0.1) +
#     ggplot2::geom_line() +
#     ggplot2::theme_minimal() +
#     ggplot2::guides(color="none") +
#     ggplot2::ylab("p [%]") +
#     ggplot2::xlab(expression(alpha))
#   
#   return(p)
# }

alpha_plot = function(acf, input_alpha, classes) {
  if (is.null(acf) | is.null(input_alpha)) return(NULL)
  
  alphas = seq(0.1, 1.0, by = 0.1)
  if (!(input_alpha %in% alphas)) {
    alphas = c(input_alpha, alphas)
  }
  
  plotdf = data.frame(alpha = alphas)
  
  
  plotdf$n = purrr::map_vec(alphas, function(a) {
    rsms = rowSums(acf[,3:ncol(acf)]>a, na.rm = TRUE) 
    rsms[rsms != 0] = classes[rsms[rsms != 0]+1]-1
    sum(rsms>acf$maxgap, na.rm = TRUE)
  })
  
  plotdf$p = plotdf$n/nrow(acf) * 100
  plotdf$color = (plotdf$alpha == input_alpha)
  
  p = ggplot2::ggplot(plotdf, ggplot2::aes(x = alpha, y = p, group=1, label = n)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(color=color)) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::geom_text(vjust = -0.3, hjust = -0.1) +
    ggplot2::theme_minimal() +
    ggplot2::guides(color="none") +
    ggplot2::ylab("p [%]") +
    ggplot2::xlab(expression(alpha))
  
  return(p)
  
}

 
## ids() function

## Returns the ids of the timeseries.
ids = function(pt, hashs) {
  pt[pt$name %in% hashs,"id"]
}

nms = function(ids, metadata) {
  print("nms fkt")
  df = purrr::map_df(metadata, function(md) {
    if (all(c("id", "name") %in% colnames(md))) {
      return(md[,c("id", "name")])
    }
  })
  print(head(df))
  nms = unique(df[df$id %in% ids,"name"])
  print(nms)
  return(nms)
}

## circle_fun() function

## Credits @joran:
## https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circle_fun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

## group() function

## Returns the ids of the timeseries.
group = function(pt, dgt, hashs) {
  gr = pt[pt$name %in% hashs,"dgroup"]
  gr = sapply(gr, function (gkey) dgt[dgt$key == gkey,"name"])
  return(gr)
}

## color() function

color = function(pt, dgt, hashs) {
  print("# Color function")
  print(dgt)
  gr = pt[pt$name %in% hashs, "dgroup"]
  gr = sapply(gr, function (gkey) jsonlite::fromJSON(dgt[dgt$key == gkey,"gparam"])[["color"]])
}

## get_timeseries() function

get_timeseries = function(db) {
  if ("selected_timeseries" %in% user.tables(db)$tablename) {
    t = get.table(db, "selected_timeseries")
    t$timestamp = as.Date(t$timestamp)
    return(t)
  } else {
    return(data.frame())
  }
}


