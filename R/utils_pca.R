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
  cap = paste("PCApp", format(Sys.Date(), "%d.%m.%Y"))
                  
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

#' avaliable_ts 
#'
#' @description A plotting utility
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

#' alpha_plot 
#'
#' @description A plotting utility
#'
#' @return A ggplot
#' 
#' @importFrom ggplot2 aes geom_line geom_point scale_color_manual geom_text 
#' @importFrom ggplot2 theme_minimal guides ylab xlab
#' @importFrom purrr map_vec
#'
#' @noRd

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


#' ids 
#'
#' @description Returns the corresponding ids of given hashs
#'
#' @param pt An copy of the primary table
#' @param hashs A vector of hashs to be eventaully replaced
#' 
#' @return A character vector containing hashs and ids
#' 
#' @importFrom ggplot2 aes geom_line geom_point scale_color_manual geom_text 
#' @importFrom ggplot2 theme_minimal guides ylab xlab
#' @importFrom purrr map_vec
#'
#' @noRd
 
ids = function(pt, hashs) {
  pt = pt[pt$name %in% hashs,c("name", "id")]
  
  hashs_rep = sapply(hashs, function(h) {
    if (h %in% pt$name) {
      return(pt[pt$name == h, "id"])
    } else {
      return(h)
    }
  })

  return(hashs_rep)
}

#' names_ids 
#'
#' @description Returns the corresponding names and ids of given hashs
#'
#' @param pt An copy of the primary table
#' @param hashs A vector of hashs to be eventaully replaced
#' 
#' @return A character vector containing hashs and ids
#' 
#' @importFrom ggplot2 aes geom_line geom_point scale_color_manual geom_text 
#' @importFrom ggplot2 theme_minimal guides ylab xlab
#' @importFrom purrr map_vec
#'
#' @noRd

names_ids = function(pt, hashs) {
  # Crop pt to columns of interest
  pt = pt[pt$name %in% hashs,c("name", "id", "clname")]
  # Replace the found hashs with name & id strings
  hashs_rep = sapply(hashs, function(h) {
    if (h %in% pt$name) {
      st = paste0(
        '"', 
        pt[pt$name == h, "clname"], 
        '" (ID: ', 
        pt[pt$name == h, "id"], 
        ')'
      )
      return(as.character(st))
    } else {
      return(h)
    }
  })
  # Return vector
  return(hashs_rep)
}

#' nms
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

nms = function(ids, metadata) {
  df = purrr::map_df(metadata, function(md) {
    if (all(c("id", "name") %in% colnames(md))) {
      return(md[,c("id", "name")])
    }
  })
  nms = unique(df[df$id %in% ids,"name"])
  return(nms)
}

#' circle_fun
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

## Credits @joran:
## https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circle_fun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#' group 
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

## Returns the ids of the timeseries.
group = function(pt, dgt, hashs) {
  gr = pt[pt$name %in% hashs,"dgroup"]
  gr = sapply(gr, function (gkey) dgt[dgt$key == gkey,"name"])
  return(gr)
}

#' color 
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

color = function(pt, dgt, hashs) {
  groups = sapply(hashs, function(h) {
    pt[pt$name == h,"dgroup"]
  })
  
  colors = sapply(groups, function(g) {
    jsonlite::fromJSON(dgt[dgt$key == g,"gparam"])[["color"]]
  })
  
  return(colors)
}



#' get_timeseries 
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

get_timeseries = function(db) {
  if ("selected_timeseries" %in% user.tables(db)$tablename) {
    t = get.table(db, "selected_timeseries")
    t$timestamp = as.Date(t$timestamp)
    return(t)
  } else {
    return(data.frame())
  }
}

#' comm_plot_stacked 
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

comm_plot_stacked = function(plotdf, label) {
  plotdf$label = sapply(1:nrow(plotdf), function(x) {
    if (plotdf$Pov[x] > 10 | plotdf$ev[x] > 0.9)  {
      paste0("PC ", x, " (", format(plotdf$csum[x], digits = 3), "%, \u03BB = ",  round(plotdf$ev[x], 2), ")")
    } else {
      ""
    }
  })
  ggplot2::ggplot(plotdf, ggplot2::aes(y=Pov, x = 0)) +
    ggplot2::geom_bar(stat="identity", position = ggplot2::position_fill(reverse = TRUE), fill = "white", color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(label = label, x = -0.4), 
      vjust = 1.6, 
      hjust = "left",
      position = ggplot2::position_fill(reverse = TRUE),
    ) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylab(paste(label, "[%]")) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, by = 0.2),
      labels = seq(0, 100, by = 20)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position="none",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(b = 20)
    )
}

#' comm_plot_bar 
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

comm_plot_bar = function(plotdf, label) {
  ggplot2::ggplot(plotdf, ggplot2::aes(x = PC, y = csum)) +
    ggplot2::geom_bar(stat="identity", col = "black", fill = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste(format(csum, digits = 3), "%")),
      vjust = 1.6
    ) +
    ggplot2::ylab(paste(label, "[%]")) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 100, by = 20)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste("\u03BB = ", format(round(ev, 2), digits = 2))),
      vjust = -1,
      color = "grey40"
    ) +
    ggplot2::theme_minimal()
}

#' plot_pcs 
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#'
#' @noRd

plot_pcs = function(df) {
  ggplot2::ggplot(data=df, ggplot2::aes(x = timestamp, y = value, colour = name)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
}

#' geo_plot_grid 
#'
#' @description Description
#' 
#' @param Param Param
#' 
#' @return Returnvalue
#' 
#' @importFrom patchwork wrap_plots
#'
#' @noRd

geo_plot_grid = function(df, PCs, roi, crs, labels) {
  
  # Helper plot function
  loadings_map_plot = function (df, PC, roi, labels, legend = FALSE) {
    
    # Colnames for plotting df
    colnames(df)[1:3] = c("name", "group", "PC")
    
    
    # Setup empty plot
    p = ggplot2::ggplot()
    # Add region of interest
    if (!is.null(roi)) {
      p = p + ggplot2::geom_sf(data = roi, fill = "white")
    }
    # Add df data
    p = p +
      ggplot2::geom_sf(
        data = df, 
        ggplot2::aes(col = PC, shape = group), 
        cex = 4
      ) + ggplot2::scale_colour_gradient2(
        low = "blue", 
        mid = "grey90", 
        high = "red", 
        limits = c(-1,1),
        name = labels[1]
      ) + ggplot2::scale_shape(
        name = labels[2]
      ) + ggplot2::ggtitle(
        PC
      ) + ggplot2::theme_minimal(
        
      ) + ggplot2::theme(
        plot.title = ggplot2::element_text(margin = ggplot2::margin(b = -10))
      )
    
    if (legend == FALSE) {
      p = p + ggplot2::theme(legend.position = "none")
    }
    
    return(p)
  }
  
  # Simple feature from df
  df = sf::st_as_sf(
    df,
    coords = c("longitude", "latitude"),
    crs = sf::st_crs(crs)
  )
  
  l = lapply(PCs, function(PC) {
    loadings_map_plot(
      df = df[,c("name", "group", PC)], 
      PC = PC,
      roi = roi,
      labels = labels
    )
  })
  
  legend = lapply(PCs, function(PC) {
    loadings_map_plot(
      df = df[,c("name", "group", PC)], 
      PC = PC,
      roi = roi,
      labels = labels,
      legend = TRUE
    )
  })
  
  if (length(PCs) >= 16) {
    grid = patchwork::wrap_plots(l, ncol = 4)
  } else {
    grid = patchwork::wrap_plots(l)
  }
  
  legend = cowplot::get_legend(patchwork::wrap_plots(legend, guides = "collect"))
  
  return(list(grid, legend))
}

