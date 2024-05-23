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
