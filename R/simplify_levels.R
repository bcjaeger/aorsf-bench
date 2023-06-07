#' @description
#'  this function is used to replace levels of a given factor
#'  with placeholders a, b, c, etc... It is useful for this
#'  particular project b/c some factors have special characters
#'  in their levels that eventually cause issues when they are
#'  one-hot or dummy encoded and passed to xgboost or glmnet.
#'
#'
simplify_levels <- function(fctr){
  levels_current <- levels(fctr)
  levels_simple <- letters[seq_along(levels_current)]
  factor(fctr, levels = levels_current, labels = levels_simple)
}
