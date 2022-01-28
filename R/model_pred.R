#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit
#' @param type
#' @param test
model_pred <- function(fit, type, test, pred_horizon) {


  message("predicting risk with ", type, " model")

  start_time <- Sys.time()

  res <- switch(
    type,

    'aorsf' = {
      predict(fit$model, new_data = test, pred_horizon = pred_horizon)
    },
    'obliqueRSF' = {
      1-predict(fit$model, newdata = test, times = pred_horizon)
    },
    'randomForestSRC' = {
      predictRisk(fit$model, newdata = test, times = pred_horizon)
    },
    'ranger' = {
      predictRisk(fit$model, newdata = test, times = pred_horizon)
    }

  )

  end_time <- Sys.time()

  list(prediction = as.numeric(res), time = end_time - start_time)

}
