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

    'cif' = {
      leaves <- predict(fit$model, newdata = test, type = 'prob')
      map_dbl(leaves, ~ 1-.x$surv[max(which(.x$time <= pred_horizon))])
    },

    'xgboost' = {

      .test <- model.matrix(~. -1L, data = test) |>
        as_tibble() |>
        select(-time, -status) |>
        as.matrix()

      1-predict(fit$model, new_data = .test, eval_times = pred_horizon)

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



