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


  orsf_prefit <- orsf(
    Surv(time, status) ~ .,
    mtry = mtry,
    split_min_obs = node_size,
    control = control,
    data = train,
    no_fit = TRUE
  )

  start_time <- Sys.time()

  print(type_simplified)

  switch(
    type_simplified,

    'aorsf' = {
      res <- orsf_train(orsf_prefit)
    },

    'cif' = {
      res <- cforest(Surv(time, status) ~ .,
                     controls = cforest_unbiased(mtry = mtry),
                     data = train)
    },

    'coxtime' = {

      res <- coxtime(
        Surv(time, status) ~ .,
        data = train,
        frac = 0.3,
        activation = "relu",
        num_nodes = c(as.integer(mtry), 8L, 4L, 2L),
        dropout = 0.1,
        early_stopping = TRUE,
        epochs = 100,
        batch_size = 32L
      )


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
                  mtry = mtry,
                  verbose = FALSE,
                  use.cv = nrow(train) < 500,
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
