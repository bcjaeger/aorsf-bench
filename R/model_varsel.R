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

  if(model %in% c('aorsf', 'aorsf_menze')){
    fit <- orsf(Surv(time, status) ~ ., data_train = train,
                control = orsf_control_cph(iter_max = 10))
  }

  switch(
    model,

    'aorsf' = {

      vi <- orsf_vi_negate(fit)

    },

    'aorsf_menze' = {

      vi <- orsf_vi_anova(fit)

    },

    'xgboost' = {

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

      fit_final <- xgboost(data = xmat, label = ymat,
                           params = list(eta = 0.05),
                           nrounds = fit_cv$best_iteration,
                           objective = 'survival:cox',
                           eval_metric = 'cox-nloglik',
                           verbose = FALSE)

      fit_shap <-
        predict(fit_final, newdata = xmat, predcontrib = TRUE) |>
        apply(MARGIN = 2, function(x) mean(abs(x)))

      vi <- fit_shap[-which(names(fit_shap)=='BIAS')] |>
        enframe() |>
        separate(name, into = c('var', 'cat'),
                 sep = '_',
                 fill = 'right') |>
        group_by(var) |>
        summarize(value = mean(value)) |>
        deframe()


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
