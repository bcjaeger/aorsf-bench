#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param type
#' @param train
model_fit <- function(type, train) {


  message("fitting ", type, " model")

  mtry <- round(sqrt(ncol(train)-2))
  node_size <- 10

  orsf_prefit <- orsf(Surv(time, status) ~ .,
                      mtry = mtry,
                      split_min_obs = node_size,
                      data = train,
                      no_fit = TRUE)

  start_time <- Sys.time()

  switch(
    type,

    'aorsf' = {
      res <- orsf_train(orsf_prefit)
    },

    'cif' = {
      res <- cforest(Surv(time, status) ~ .,
                     controls = cforest_unbiased(mtry = mtry),
                     data = train)
    },

    'xgboost' = {
      xmat <- model.matrix(~. -1, data = train) |>
        as_tibble() |>
        as_sgb_data(status = status, time = time)

      res <- try(
        sgb_fit(sgb_df = xmat,
                verbose = 0,
                params = list(eta = 0.01,
                              objective = 'survival:cox',
                              eval_metric = 'cox-nloglik'))
      )

      # sometimes you don't have enough events to do CV
      if(inherits(res, 'try-error'))
        res <- sgb_fit(sgb_df = xmat,
                       verbose = 0,
                       nrounds = 250,
                       params = list(eta = 0.01,
                                     objective = 'survival:cox',
                                     eval_metric = 'cox-nloglik'))

    },

    'obliqueRSF' = {
      res <- ORSF(train,
                  ntree = 500,
                  verbose = FALSE,
                  mtry = mtry,
                  min_obs_to_split_node = node_size)
    },

    'randomForestSRC' = {
      res <- rfsrc(Surv(time, status) ~ .,
                   data = train,
                   mtry = mtry,
                   nodesize = node_size)
    },

    'ranger' = {
      res <- ranger(
        Surv(time, status) ~ .,
        data = train,
        mtry = mtry,
        min.node.size = node_size
      )
    }

  )

  end_time <- Sys.time()

  list(model = res, time = end_time - start_time)

}
