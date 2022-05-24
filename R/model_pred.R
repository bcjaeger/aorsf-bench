#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit
#' @param type
#' @param test

aorsf_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  res <- predict(object$fit,
                 new_data = test,
                 pred_horizon = pred_horizon)

  end_time <- Sys.time()

  object$pred <- as.numeric(res)
  object$time_pred <- end_time - start_time
  object

}


aorsf_cph_15_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

aorsf_cph_1_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

aorsf_net_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

cif_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  leaves <- predict(object$fit, newdata = test, type = 'prob')
  res <- map_dbl(leaves, ~ 1-.x$surv[max(which(.x$time <= pred_horizon))])

  end_time <- Sys.time()

  object$pred <- as.numeric(res)
  object$time_pred <- end_time - start_time
  object

}

coxtime_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  pmat <- predict(object$fit,
                  newdata = test,
                  type = 'survival')

  pmat_times <- as.numeric(colnames(pmat))

  pmat_col <- max(which(pmat_times <= pred_horizon))

  res <- 1-pmat[, pmat_col, drop = TRUE]

  end_time <- Sys.time()

  object$pred <- as.numeric(res)
  object$time_pred <- end_time - start_time
  object

}

obliqueRSF_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  res <- 1-predict(object$fit,
                   newdata = test,
                   times = pred_horizon)

  end_time <- Sys.time()

  object$pred <- as.numeric(res)
  object$time_pred <- end_time - start_time
  object

}

xgboost_pred <- function(object, test, pred_horizon){

  .test <- model.matrix(~. -1L, data = test) |>
    as_tibble() |>
    select(-time, -status) |>
    as.matrix()

  start_time <- Sys.time()

  res <- 1 - predict(object$fit,
                     new_data = .test,
                     eval_times = pred_horizon)

  # sometimes the base hazard can't be estimated
  if(all(is.na(res))){
    res <- xgboost:::predict.xgb.Booster(
      object = object$fit$fit,
      newdata = .test
    ) |>
      scales::rescale(to = c(0,1))
  }

  end_time <- Sys.time()

  object$pred <- as.numeric(res)
  object$time_pred <- end_time - start_time
  object

}

randomForestSRC_pred <- function(object, test, pred_horizon){

  ranger_pred(object, test, pred_horizon)

}

ranger_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  res <- predictRisk(object$fit,
                     newdata = test,
                     times = pred_horizon)

  end_time <- Sys.time()

  object$pred <- as.numeric(res)
  object$time_pred <- end_time - start_time
  object

}

