# simple wrapper for attribute adding

add_attribute <- function(x, attr_name = "key", attr_value = list()) {

  attr(x, attr_name) <- attr_value

  x

}
