#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param attr_name
#' @param attr_value
add_attribute <- function(x, attr_name = "key", attr_value = list()) {

  attr(x, attr_name) <- attr_value

  x

}
