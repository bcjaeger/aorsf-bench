#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model
#' @param train
model_varsel <- function(model, train) {

  stopifnot(ncol(train) > 2)

  message("computing variable importance using ", model)

  start_time <- Sys.time()

  switch(
    model,

    'aorsf' = {

      fit <- orsf(Surv(time, status) ~ .,
                  data_train = train,
                  importance = TRUE)

      vi <- fit$importance


    },

    'aorsf_menze' = {

      fit <- orsf(Surv(time, status) ~ ., data_train = train,
                  control = orsf_control_cph(iter_max = 20))

      vi <- orsf_vi_pvalue(fit)

    },

    'aorsf_pv' = {

      fit <- orsf(Surv(time, status) ~ ., data_train = train,
                  control = orsf_control_cph(iter_max = 20))
      vi <- aorsf:::orsf_vi_pv(fit)

    },

    'xgboost' = {

      xmat <- as.matrix(select(train, -time, -status))
      ymat <- train$time
      ymat[train$status == 0] <- ymat[train$status == 0] * (-1)

      fit_cv <- xgb.cv(data = xmat,
                       params = list(eta = 0.05),
                       label = ymat, nfold=10,
                       nrounds = 500,
                       objective = 'survival:cox',
                       eval_metric = 'cox-nloglik',
                       verbose = FALSE,
                       early_stopping_rounds = 25)

      fit_final <- xgboost(data = xmat, label = ymat,
                           params = list(eta = 0.02),
                           nrounds = fit_cv$best_iteration,
                           objective = 'survival:cox',
                           eval_metric = 'cox-nloglik',
                           verbose = FALSE)

      fit_shap <-
        predict(fit_final, newdata = xmat, predcontrib = TRUE) |>
        apply(MARGIN = 2, function(x) mean(abs(x)))

      vi <- fit_shap[-which(names(fit_shap)=='BIAS')]


    },

    'randomForestSRC' = {
      fit <- rfsrc(Surv(time, status) ~ ., data = train, importance = TRUE)
      vi <- fit$importance

    },
    'ranger' = {
      fit <- ranger(
        Surv(time, status) ~ ., data = train,
        importance = 'permutation',
        splitrule = 'extratrees'
      )
      vi <- fit$variable.importance

    }

  )

  end_time <- Sys.time()

  list(vi = sort(vi, decreasing = TRUE), time = end_time - start_time)

}
