#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_type
model_new <- function(model_type) {

  out <- list(model_fit = NULL,
              model_pred = NULL,
              time_fit = NULL,
              time_pred = NULL)

  attr(out, 'model_type') <- model_type

  class(out) <- str_extract(model_type, '[a-zA-Z]+')

  out

}
