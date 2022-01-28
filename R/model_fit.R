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
