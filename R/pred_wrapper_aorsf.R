
#' this is a wrapper for aorsf's prediction function that is intended
#' to be used in combination with the fastshap R package. It's not included
#' in the current analysis because I'm not sure if shapley approximation
#' values are intended to be computed for algos that compute predicted
#' risk probabilities. Some preliminary work I did suggested these types
#' of shapley values are less effective at separating signal from noise
#' variables compared to negation and permutation importance for the
#' oblique RSF.

# Prediction wrapper
pred_wrapper_aorsf <- function(object, newdata) {
  predict(object,
          new_data = newdata,
          pred_horizon = 2.5) |>
    as.numeric()
}
