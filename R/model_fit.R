#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param type
#' @param train
model_fit <- function(type, train) {


  message("fitting ", type, " model")

  # params shared by all tree models
  mtry <- round(sqrt(ncol(train)-2))
  node_size <- 10

  aorsf_cph_iter <- 1

  type_simplified <- type

  control = orsf_control_net()

  if(str_detect(type, '^aorsf')){

    aorsf_cph_iter = as.numeric(str_extract(type, '\\d+'))

    if(!is.na(aorsf_cph_iter)) {
      control = orsf_control_cph(iter_max = aorsf_cph_iter)
    }

    type_simplified <- 'aorsf'

  }

  start_time <- Sys.time()

  switch(

    type_simplified,

    'aorsf' = {
      res <- orsf(
        data_train = train,
        formula = Surv(time, status) ~ .,
        mtry = mtry,
        n_retry = 3,
        split_min_obs = node_size,
        control = control,
        oobag_pred = FALSE
      )

    },

    'cif' = {
      res <- cforest(Surv(time, status) ~ .,
                     controls = cforest_unbiased(mtry = mtry),
                     data = train)
    },

    'coxtime' = {


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

      res <- coxtime(
        Surv(time, status) ~ .,
        data = train,
        activation = "relu",
        num_nodes = nn[[which.min(scores)]],
        dropout = 0.1,
        epochs = epochs,
        batch_size = 32L
      )

    },

    'xgboost' = {
      xmat <- model.matrix(~. -1, data = train) |>
        as_tibble() |>
        as_sgb_data(status = status, time = time)

      params <- list(eta = 0.01,
           objective = 'survival:cox',
           eval_metric = 'cox-nloglik',
           # min_child_weight = node_size,
           colsample_bynode = mtry / ncol(xmat$data))

      cv_fit <- xgboost::xgb.cv(params = params,
                                data = xmat$data,
                                label = xmat$label,
                                nfold = 3,
                                nround = 5000,
                                early_stopping_rounds = 25,
                                verbose = FALSE)

      # res <- try(
      #   sgb_fit(sgb_df = xmat,
      #           verbose = 0,
      #           params = params)
      # )

      # sometimes you don't have enough events to do CV
      if(inherits(cv_fit, 'try-error'))
        res <- sgb_fit(sgb_df = xmat,
                       verbose = 0,
                       nrounds = 100,
                       params = params)
      else
        res <- sgb_fit(sgb_df = xmat,
                       verbose = 0,
                       nrounds = cv_fit$best_iteration,
                       params = params)

    },

    'obliqueRSF' = {
      res <- ORSF(train,
                  ntree = 500,
                  mtry = mtry,
                  verbose = FALSE,
                  use.cv = nrow(train) < 500,
                  min_obs_to_split_node = node_size)
    },

    'randomForestSRC' = {
      res <- rfsrc(Surv(time, status) ~ .,
                   ntree = 500,
                   samptype = 'swr',
                   perf.type = 'none',
                   data = train,
                   mtry = mtry,
                   nodesize = node_size)
    },

    'ranger' = {
      res <- ranger(
        Surv(time, status) ~ .,
        num.trees = 500,
        splitrule = 'extratrees',
        data = train,
        mtry = mtry,
        oob.error = FALSE,
        min.node.size = node_size
      )
    }

  )

  end_time <- Sys.time()

  list(model = res, time = end_time - start_time)

}
