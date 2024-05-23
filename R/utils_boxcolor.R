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
