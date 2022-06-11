#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fctr
simplify_levels <- function(fctr){
  levels_current <- levels(fctr)
  levels_simple <- letters[seq_along(levels_current)]
  factor(fctr, levels = levels_current, labels = levels_simple)
}
