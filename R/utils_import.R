#' boxcolor
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

boxcolor = function(boxid, col) {
  
  # Raw css. The #'s mark substrings to be replaced.
  css = "
  #myid >.col-sm-12>.box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#mycolor
  }

  #myid >.col-sm-12>.box.box-solid.box-primary{
    border-bottom-color:.mycolor;
    border-left-color:.mycolor;
    border-right-color:.mycolor;
    border-top-color:.mycolor;
  }
  "
  
  # Replace box id and color in css code.
  css = gsub("myid", boxid, css)
  css = gsub(".mycolor", col, css)
  
  return(css)
  
}

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
    d = do.call(readm, g)
    str(d)
    return(d)
  }, error = function(e) {
    return(data.frame())
  })
}

#' head_data
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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

#' join_timeseries_with_metadata
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# This function definitely needs some clean up. But it works.

join_timeseries_with_metadata = function(db) {
  
  user_tables = user.tables(db)$tablename
  
  primary_table = get.table(db, "primary_table")
  
  timeseries = primary_table[primary_table$dtype == "Timeseries", "name"]
  
  metadata = primary_table[primary_table$dtype == "Metadata", "name"]
  metadata = metadata[sapply(metadata, function(n) paste0(n, "_clean") %in% user_tables)]
  
  if (length(metadata)>0) {
    ids = unique(dplyr::bind_rows(lapply(metadata, function(md) {
      get.table(db, paste0(md, "_clean"))[,c("id", "name")]
    })))
    
    matched_ids = sapply(timeseries, function(ts) {
      if (paste0(ts, "_head") %in% user_tables) {
        head = toString(get.table(db, paste0(ts, "_head"))$data)
      } else {
        return(NULL)
      }
      matches = sapply(ids$id, grepl, x = head)
      if (any(matches)) {
        return(ids$id[matches][1])
      } else {
        return(NULL)
      }
    })
    
    matched_ids = unlist(matched_ids)
    matched_ids = matched_ids[!is.null(unname(matched_ids))]
    
    if (is.null(matched_ids)) {
      return(NULL)
    }
    
    clname = sapply(unname(matched_ids), function (id) {
      if (!is.na(ids[ids$id == id, "name"])) {
        return(ids[ids$id == id, "name"])
      } else {
        return(NA)
      }
    })
      
    # ids[ids$id %in% , "name"]
    
    matched_ids = trimws(format(matched_ids, scientific = FALSE))
    matched_ids = data.frame(name = names(matched_ids), id = unname(matched_ids), clname = clname)
    primary_table = primary_table[,!(colnames(primary_table) %in% c("id", "clname"))]
    primary_table = merge(primary_table, matched_ids, by = "name", all.x = TRUE)
    
    lapply(primary_table$key, function(key) {
      change.tablevalue(db, "primary_table", key, "id", primary_table[primary_table$key == key,"id"])
      change.tablevalue(db, "primary_table", key, "clname", primary_table[primary_table$key == key, "clname"])
    })
    
    colnames(matched_ids) = c("hash", "id")
    
    # Create a list of metadata for the matched ids
    metadata_for_global = lapply(metadata, function(n) {
      md = get.table(db, paste0(n, "_clean"))
      if ("id" %in% colnames(md)) {
        md$id = trimws(format(md$id, scientific = FALSE))
        md = merge(matched_ids, md, by = "id", all.x = TRUE)
        return(md)
      } else {
        return(NULL)
      }
    })

    # Return the list in order to store it in global variable r
    return(metadata_for_global)
    
  }
  
}

#' optional_fun_param
#'
#' @description Return optional parameters of a function
#'
#' @return List of optional parameters
#'
#' @importFrom purrr map_vec
#'
#' @noRd

optional_fun_param = function(funname) {
  if (!is.null(funname)) {
    form = formals(funname)
    okparam = c("character", "integer", "logical", "numeric", "integer")
    opt = purrr::map_vec(form, function(f) class(f) %in% okparam)
    return(form[opt])
  }
}

#' random_address
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

random_address = function() {
  paste0(sample(letters, 1), sample(c(letters, 0:9), 9), collapse = "")
}

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

#' recreateDataObjects
#'
#' @description Build data objects from entries in datagroup table
#'
#' @importFrom methods slot
#'
#' @return Object s of class data
#'
#' @noRd

recreateDataObjects = function(...) {
  # Build list from ananymous arguments
  entry = data.frame(...)
  # Select future object attributes
  att = entry[!(colnames(entry) %in% names(formals(entry$readmethod)))]
  # Select objects readparameters
  opt = entry[colnames(entry) %in% names(formals(entry$readmethod))]
  # Instantiate object with readparameters
  newobject = methods::new(entry$dtype, readparam = opt)
  # Assign slot values
  # lapply(colnames(att), function(n) slot(newobject, n) = att[1,n])
  
  for (cn in colnames(att)) {
    methods::slot(newobject, cn) = att[,cn]
  }
  
  return(newobject)
}
