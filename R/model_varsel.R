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

  model_fitter <- str_remove(model, '-.*$')

  if(model_fitter == 'aorsf'){

    fit <- orsf(Surv(time, status) ~ ., data_train = train,
                control = orsf_control_cph(iter_max = 10))

  }

  if(model_fitter == 'xgboost'){

    xmat <- train |>
      as.data.table() |>
      one_hot() |>
      select(-time, -status) |>
      as.matrix()

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

    fit <- xgboost(data = xmat, label = ymat,
                   params = list(eta = 0.05),
                   nrounds = fit_cv$best_iteration,
                   objective = 'survival:cox',
                   eval_metric = 'cox-nloglik',
                   verbose = FALSE)

  }

  switch(
    model,

    'aorsf-negation' = {

      vi <- orsf_vi_negate(fit)

    },

    'aorsf-anova' = {

      vi <- orsf_vi_anova(fit)

    },

    'aorsf-shap' = {

      data_shap <- as.data.frame(select(train, -time, -status))

      # Compute approximate Shapley values using 10 Monte Carlo repetitions
      shap <- fastshap::explain(fit,
                                X = data_shap,
                                newdata = data_shap,
                                pred_wrapper = pred_wrapper_aorsf,
                                nsim = 10)

      vi <- apply(shap, 2, function(x) mean(abs(x)))

    },


    'xgboost-shap' = {

      vi <-
        predict(fit, newdata = xmat, predcontrib = TRUE) |>
        apply(MARGIN = 2, function(x) mean(abs(x)))

      vi <- vi[-which(names(vi)=='BIAS')]


    },

    'xgboost-gain' = {

      vi <- xgb.importance(model = fit) %>%
        select(Feature, Gain) %>%
        deframe()

    },

    'randomForestSRC-permutation' = {

      fit <- rfsrc(Surv(time, status) ~ ., data = train, importance = TRUE)
      vi <- fit$importance

    }

  )

  end_time <- Sys.time()

  list(vi = sort(vi, decreasing = TRUE), time = end_time - start_time)

}
