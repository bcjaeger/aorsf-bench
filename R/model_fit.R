#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param type
#' @param train
model_fit <- function(type, train) {


  message("fitting ", type, " model")

  start_time <- Sys.time()

  switch(
    type,

    'aorsf' = {
      res <- orsf(Surv(time, status) ~ ., data = train)
    },

    'xgboost' = {


      xmat <- model.matrix(~. -1, data = train) |>
        as_tibble() |>
        as_sgb_data(status = status, time = time)


      res <- sgb_fit(sgb_df = xmat,
                     verbose = 0,
                     params = list(max_depth=2,
                                   eta = 0.01,
                                   objective = 'survival:cox',
                                   eval_metric = 'cox-nloglik'))

    },

    'obliqueRSF' = {
      res <- ORSF(train, ntree = 500, verbose = FALSE)
    },

    'randomForestSRC' = {
      res <- rfsrc(Surv(time, status) ~ ., data = train)
    },

    'ranger' = {
      res <- ranger(
        Surv(time, status) ~ ., data = train, splitrule = 'extratrees'
      )
    }

  )

  end_time <- Sys.time()


  list(model = res, time = end_time - start_time)

}
