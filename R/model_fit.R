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

rotsf_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  train_onehot <- model.matrix(~. -1L, data = train) |>
    as.data.frame()

  start_time <- Sys.time()

  fit <- rotsfpca(
    formula = Surv(time, status) ~ .,
    data = train_onehot
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

rsfse_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  train_onehot <- model.matrix(~. -1L, data = train) |>
    as.data.frame()

  start_time <- Sys.time()

  xnames <- setdiff(names(train_onehot), c('time', 'status'))

  fit <- rsfes(
    x = train_onehot[, xnames],
    y = Surv(time = train_onehot$time,
             event = train_onehot$status)
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

aorsf_cph_1_filter_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  split_min_events <- min( round(sum(train$status) / 5), 5)

  start_time <- Sys.time()

  # fit_filter <- orsf(
  #   data_train = train,
  #   formula = Surv(time, status) ~ .,
  #   n_tree = 100,
  #   mtry = mtry,
  #   n_retry = 3,
  #   split_min_obs = node_size,
  #   split_min_events = split_min_events,
  #   control = orsf_control_cph(iter_max = 1,
  #                              do_scale = FALSE),
  #   oobag_pred = FALSE
  # )
  #
  # vi <- orsf_vi_anova(fit_filter)
  #
  # n_keep <- round( length(vi) * 0.75 )
  #
  # keep <- c('time', 'status', names(vi)[seq(n_keep)])
  #
  # fit <- orsf(
  #   data_train = train[, keep],
  #   formula = Surv(time, status) ~ .,
  #   mtry = mtry,
  #   n_retry = 3,
  #   split_min_obs = node_size,
  #   split_min_events = split_min_events,
  #   control = orsf_control_cph(iter_max = 1,
  #                              do_scale = FALSE),
  #   oobag_pred = FALSE
  # )

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    split_min_events = split_min_events,
    control = orsf_control_cph(iter_max = 1,
                               do_scale = FALSE,
                               pval_max = .25),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

aorsf_cph_1_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  split_min_events <- min( round(sum(train$status) / 5), 5)

  start_time <- Sys.time()

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    split_min_events = split_min_events,
    control = orsf_control_cph(iter_max = 1,
                               do_scale = FALSE),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

aorsf_random_fit <- function(train, node_size = 10, ...){

  beta_fun <- function(x_node, y_node, w_node) {
    matrix(runif(ncol(x_node)), ncol=1)
  }

  mtry <- round(sqrt(ncol(train)-2))

  split_min_events <- min( round(sum(train$status) / 5), 5)

  start_time <- Sys.time()

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    split_min_events = split_min_events,
    control = orsf_control_custom(beta_fun = beta_fun),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

aorsf_cph_15_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  split_min_events <- min( round(sum(train$status) / 5), 5)

  start_time <- Sys.time()

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    split_min_events = split_min_events,
    control = orsf_control_cph(iter_max = 15,
                               do_scale = TRUE),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

aorsf_net_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  df_target <- max(mtry - 2, round(mtry/2), 2)

  split_min_events <- min( round(sum(train$status) / 5), 5)

  start_time <- Sys.time()

  fit <- orsf(
    data_train = train,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    split_min_events = split_min_events,
    control = orsf_control_net(df_target = df_target),
    oobag_pred = FALSE
  )

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

randomForestSRC_fit <- function(train, node_size = 10, ...){

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

ranger_fit <- function(train, node_size = 10, ...){

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

cif_fit <- function(train, node_size = 10, ...){

  mtry <- round(sqrt(ncol(train)-2))

  start_time <- Sys.time()

  fit <- cforest(Surv(time, status) ~ .,
                 controls = cforest_unbiased(mtry = mtry),
                 data = train)

  end_time <- Sys.time()

  list(fit = fit, time_fit = end_time - start_time)

}

coxtime_fit <- function(train, node_size = 10, ...){

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
  ) |>
    lapply(round, digits = 0)

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

obliqueRSF_fit <- function(train, node_size = 10, ...){

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

xgb_cox_fit <- function(train,
                    node_size = 10,
                    pred_horizon,
                    ...){

  mtry <- round(sqrt(ncol(train)-2))

  xmat <- model.matrix(~. -1L, data = select(train, -time, -status))

  ymat <- fifelse(
    test = train$status == 1,
    yes = train$time,
    no = train$time * (-1)
  )

  dtrain <- xgb.DMatrix(data = xmat, label = ymat)

  params <- list(eta = 0.01,
                 objective = 'survival:cox',
                 eval_metric = 'cox-nloglik',
                 min_child_weight = node_size,
                 colsample_bynode = mtry / ncol(xmat))

  start_time <- Sys.time()

  cv_fit <- xgb.cv(params = params,
                   data = dtrain,
                   nfold = if(nrow(xmat < 100)) 2 else 5,
                   nround = 5000,
                   early_stopping_rounds = 25,
                   verbose = FALSE)

  fit <- xgb.train(params = params,
                   data = dtrain,
                   nrounds = cv_fit$best_iteration)

  # baseline hazard estimate at pred horizon
  lin_preds <- predict(fit, newdata = xmat)

  base_haz <-
    gbm::basehaz.gbm(t = train[, 'time'],
                     delta = train[, 'status'],
                     f.x = lin_preds,
                     t.eval = pred_horizon,
                     smooth = TRUE,
                     cumulative = TRUE)

  end_time <- Sys.time()

  list(fit = fit,
       time_fit = end_time - start_time,
       base_haz = base_haz)

}

xgb_aft_fit <- function(train,
                        node_size = 10,
                        pred_horizon,
                        ...){

  mtry <- round(sqrt(ncol(train)-2))

  xmat <- model.matrix(~. -1L, data = select(train, -time, -status))

  y_lower <- train$time
  y_upper <- fifelse(
    test = train$status == 1,
    yes = train$time,
    no = Inf
  )

  dtrain <- xgb.DMatrix(data = xmat)

  setinfo(dtrain, 'label', y_lower)
  setinfo(dtrain, 'label_lower_bound', y_lower)
  setinfo(dtrain, 'label_upper_bound', y_upper)

  params <- list(eta = 0.01,
                 objective = 'survival:aft',
                 eval_metric = 'aft-nloglik',
                 aft_loss_distribution = 'normal',
                 aft_loss_distribution_scale = 1.20,
                 min_child_weight = node_size,
                 colsample_bynode = mtry / ncol(xmat))

  start_time <- Sys.time()

  cv_fit <- xgb.cv(params = params,
                   data = dtrain,
                   nfold = 5,
                   nround = 5000,
                   early_stopping_rounds = 25,
                   verbose = FALSE)

  fit <- xgb.train(params = params,
                   data = dtrain,
                   nrounds = cv_fit$best_iteration)

  end_time <- Sys.time()

  list(fit = fit,
       time_fit = end_time - start_time)

}




cox_net_fit <- function(train, node_size = 10, pred_horizon, ...){

  xmat <- model.matrix(~. -1,
                       data = select(train, -time, -status))

  ymat <- as.matrix(select(train, time, status))

  start_time <- Sys.time()

  fit <- cv.glmnet(x = xmat,
                   y = ymat,
                   alpha = 1/2,
                   nfolds = 5,
                   family = 'cox')

  # baseline hazard estimate at pred horizon
  lin_preds <- predict(fit$glmnet.fit,
                       newx = xmat,
                       s = fit$lambda.min,
                       type = 'link')

  base_haz <-
    gbm::basehaz.gbm(t = train[, 'time'],
                     delta = train[, 'status'],
                     f.x = lin_preds,
                     t.eval = pred_horizon,
                     smooth = TRUE,
                     cumulative = TRUE)

  end_time <- Sys.time()

  list(fit = fit,
       time_fit = end_time - start_time,
       base_haz = base_haz)

}




