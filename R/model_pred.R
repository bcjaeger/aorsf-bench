#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit
#' @param type
#' @param test


rotsf_pred <- function(object, test, pred_horizon){

  test_onehot <- model.matrix(~. -1L, data = test) |>
    as.data.frame()

  start_time <- Sys.time()

  res <- rotsfpca.surv_predict(object$fit,
                               newdata = select(test_onehot, -time, -status),
                               uniquetimes = pred_horizon)

  end_time <- Sys.time()

  list(
    pred = 1 - res,
    time = end_time - start_time
  )

}

rsfse_pred <- function(object, test, pred_horizon){

  test_onehot <- model.matrix(~. -1L, data = test) |>
    as.data.frame()

  start_time <- Sys.time()

  res <- rsfes.surv_predict(object$fit,
                            newdata = select(test_onehot, -time, -status),
                            uniquetimes = pred_horizon)

  end_time <- Sys.time()

  list(
    pred = 1 - res,
    time = end_time - start_time
  )

}




aorsf_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  res <- predict(object$fit,
                 new_data = test,
                 pred_horizon = pred_horizon)

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

}

aorsf_random_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

aorsf_fast_filter_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

aorsf_cph_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

aorsf_fast_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

aorsf_net_pred <- function(object, test, pred_horizon){
  aorsf_pred(object, test, pred_horizon)
}

cox_net_pred <- function(object, test, pred_horizon){

  res <- matrix(nrow = nrow(test), ncol = length(pred_horizon))


  .test <- model.matrix(~. -1L, data = test) |>
    as_tibble() |>
    select(-time, -status) |>
    as.matrix()

  start_time <- Sys.time()

  lin_preds <- predict(object$fit$glmnet.fit,
                       newx = .test,
                       s = object$fit$lambda.min,
                       type = 'link')

  for(i in seq_along(pred_horizon)){
    res[, i] <- exp(exp(lin_preds) * -object$base_haz[i])
  }

  end_time <- Sys.time()

  list(
    pred = 1 - res,
    time = end_time - start_time
  )

}

cif_pred <- function(object, test, pred_horizon){

  # browser()

  start_time <- Sys.time()

  person_surv_objects <- predict(object$fit, newdata = test, type = 'prob')

  res <- matrix(0, nrow = nrow(test), ncol = length(pred_horizon))

  for(t in seq_along(pred_horizon)){
    res[, t] <- map_dbl(
      .x = person_surv_objects,
      .f = ~ {

        if(min(.x$time > pred_horizon[t])) return(0)

        1 - .x$surv[max(which(.x$time <= pred_horizon[t]))]

      }
    )
  }

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

}

coxtime_pred <- function(object, test, pred_horizon){

  res <- matrix(nrow = nrow(test), ncol = length(pred_horizon))

  start_time <- Sys.time()

  pmat <- predict(object$fit,
                  newdata = test,
                  type = 'survival')

  pmat_times <- as.numeric(colnames(pmat))

  for(i in seq_along(pred_horizon)){
    pmat_col <- max(which(pmat_times <= pred_horizon[i]))
    res[, i] <- pmat[, pmat_col, drop = TRUE]
  }

  end_time <- Sys.time()

  list(
    pred = 1 - res,
    time = end_time - start_time
  )

}

obliqueRSF_pred <- function(object, test, pred_horizon){

  start_time <- Sys.time()

  res <- 1-predict(object$fit,
                   newdata = test,
                   times = pred_horizon)

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

}

xgb_cox_pred <- function(object, test, pred_horizon){

  .test <- model.matrix(~. -1L, data = select(test, -time, -status))

  res <- matrix(nrow = nrow(test), ncol = length(pred_horizon))

  start_time <- Sys.time()

  lin_preds <- predict(object$fit, newdata = .test)

  for(i in seq_along(pred_horizon)){
    res[, i] <- exp(exp(lin_preds) * -object$base_haz[i])
  }

  end_time <- Sys.time()

  list(
    pred = 1-res,
    time = end_time - start_time
  )

}

xgb_aft_pred <- function(object, test, pred_horizon){

  .test <- model.matrix(~. -1L, data = select(test, -time, -status))

  res <- matrix(nrow = nrow(test), ncol = length(pred_horizon))

  start_time <- Sys.time()

  lin_preds <- predict(object$fit, newdata = .test)
  lin_preds <- rescale(-lin_preds, to = c(0.1, 0.99))

  for(i in seq_along(pred_horizon)){
    res[, i] <- lin_preds
  }

  end_time <- Sys.time()

  list(
    pred = res,
    time = end_time - start_time
  )

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

  list(
    pred = res,
    time = end_time - start_time
  )

}

