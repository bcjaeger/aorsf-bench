#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param type
#' @param train


# template to be used for writing specific functions
# model_fit <- function(train, node_size = 10){
#
  # mtry <- round(sqrt(ncol(train)-2))
  #
  # start_time <- Sys.time()
  #
  # # code for fitting
  #
  # end_time <- Sys.time()
  #
  # list(fit = fit, time_fit = end_time - start_time)
#
# }

aorsf_cph_1_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    control = orsf_control_cph(iter_max = 1),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

aorsf_cph_15_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    control = orsf_control_cph(iter_max = 15),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

aorsf_net_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  df_target <- max(mtry - 2,
                   round(mtry/2),
                   2)

  start_time <- Sys.time()

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    control = orsf_control_net(df_target = df_target),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

randomForestSRC_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- rfsrc(Surv(time, status) ~ .,
               ntree = 500,
               samptype = 'swr',
               perf.type = 'none',
               data = train,
               mtry = mtry,
               nodesize = node_size)

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

ranger_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- ranger(
    Surv(time, status) ~ .,
    num.trees = 500,
    splitrule = 'extratrees',
    data = train,
    mtry = mtry,
    oob.error = FALSE,
    min.node.size = node_size
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

cif_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- cforest(Surv(time, status) ~ .,
                 controls = cforest_unbiased(mtry = mtry),
                 data = train)

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

coxtime_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  nn <- list(
    c(mtry),
    c(mtry, mtry/2),
    c(mtry, mtry/2 , mtry/2),
    c(mtry, mtry/2, mtry/2 , mtry/2),
    c(mtry, mtry, mtry/2, mtry/2 , mtry/2),
    c(mtry, mtry, mtry, mtry/2, mtry/2 , mtry/2),
    c(mtry, mtry, mtry, mtry, mtry/2, mtry/2 , mtry/2),
    c(mtry, mtry, mtry, mtry, mtry, mtry/2, mtry/2 , mtry/2)
  )

  tuners <- map(
    .x = nn,
    .f = ~ coxtime(
      Surv(time, status) ~ .,
      data = train,
      frac = 0.25,
      activation = "relu",
      num_nodes = .x,
      dropout = 0.1,
      early_stopping = TRUE,
      epochs = 500,
      batch_size = 32L
    )
  )

  scores <- tuners |>
    map_dbl(~min(.x$model$val_metrics$scores$loss$score))

  winner_loss_vals <-
    tuners[[which.min(scores)]]$model$val_metrics$scores$loss$score

  epochs <-
    tuners[[which.min(scores)]]$model$val_metrics$scores$loss$epoch[
      which.min(winner_loss_vals)
    ]

  fit <- coxtime(
    Surv(time, status) ~ .,
    data = train,
    activation = "relu",
    num_nodes = nn[[which.min(scores)]],
    dropout = 0.1,
    epochs = epochs,
    batch_size = 32L
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

obliqueRSF_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- ORSF(train,
              ntree = 500,
              mtry = mtry,
              verbose = FALSE,
              use.cv = nrow(train) < 500,
              min_obs_to_split_node = node_size)

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

xgboost_fit <- function(train, node_size = 10){

  mtry <- round(sqrt(ncol(train)-2))

  xmat <- model.matrix(~. -1, data = train) |>
    as_tibble() |>
    as_sgb_data(status = status, time = time)

  params <- list(eta = 0.01,
                 objective = 'survival:cox',
                 eval_metric = 'cox-nloglik',
                 # min_child_weight = node_size,
                 colsample_bynode = mtry / ncol(xmat$data))

  start_time <- Sys.time()

  cv_fit <- xgb.cv(params = params,
                   data = xmat$data,
                   label = xmat$label,
                   nfold = 5,
                   nround = 5000,
                   early_stopping_rounds = 25,
                   verbose = FALSE)

  # sometimes you don't have enough events to do CV
  if(inherits(cv_fit, 'try-error'))
    fit <- sgb_fit(sgb_df = xmat,
                   verbose = 0,
                   nrounds = 100,
                   params = params)
  else
    fit <- sgb_fit(sgb_df = xmat,
                   verbose = 0,
                   nrounds = cv_fit$best_iteration,
                   params = params)

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

