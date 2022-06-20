

# Prediction wrapper
pred_wrapper_aorsf <- function(object, newdata) {
  predict(object,
          new_data = newdata,
          pred_horizon = 2.5) |>
    as.numeric()
}
