#' instantiatePostgreSQL
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

instantiatePostgreSQL = function(cred, superuser = FALSE) {
  if (superuser) {
    pg = methods::new(
      Class = "PostgreSQL",
      host = getElement(object = cred, name = "host"),
      port = getElement(object = cred, name = "port"),
      user = getElement(object = cred, name = "superuser"),
      password = getElement(object = cred, name = "superpassword"),
      dbname = getElement(object = cred, name = "dbname")
    )
    return(pg)
  } else {
    pg = methods::new(
      Class = "PostgreSQL",
      host = getElement(object = cred, name = "host"),
      port = getElement(object = cred, name = "port"),
      user = getElement(object = cred, name = "user"),
      password = getElement(object = cred, name = "password"),
      dbname = getElement(object = cred, name = "dbname")
    )
    return(pg)
  }
}
