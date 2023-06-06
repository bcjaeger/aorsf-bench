

#' @description benchmark experiment with simulated data
#'
#' @param data_source character value indicating which dataset to load
#' @param model_type character value indicating which model to use
#' @param data_load_fun function for loading data
#' @param model_fit_fun function for model fitting
#' @param model_pred_fun function for model predictions
#' @param n_obs number of observations in training data
#' @param pred_corr_max maximum correlation magnitude between predictors
#' @param run_seed random seed value

bench_pred_sim <- function(data_source,
                           model_type,
                           data_load_fun,
                           model_fit_fun,
                           model_pred_fun,
                           n_obs = NULL,
                           pred_corr_max = NULL,
                           run_seed) {

  set.seed(run_seed)

  data_all <- data_load_fun(n_obs = n_obs,
                            pred_corr_max = pred_corr_max,
                            n_pred_junk = 0,
                            eff_size_pred_main = 1/3,
                            eff_size_intr_main = 1/3,
                            eff_size_pred_nlin = 1/3,
                            eff_size_intr_nlin = 1/3,
                            eff_size_pred_cmbn = 1/3,
                            eff_size_intr_cmbn = 1/3)

  data_all <- data_all$data

  train <- data_all

  test <- sim_surv(n_obs = 5000,
                   pred_corr_max = pred_corr_max) |>
    getElement('data')

  imputer <- recipe(x = train, time + status ~ .) |>
    step_impute_mean(all_numeric_predictors()) |>
    step_impute_mode(all_nominal_predictors()) |>
    step_nzv(all_predictors()) |>
    step_range(all_numeric_predictors()) |>
    prep()

  .train <- as.data.frame(juice(imputer))
  .test <- as.data.frame(bake(imputer, new_data = test))

  event_time_bounds <- quantile(
    x = data_all$time[data_all$status==1],
    probs = c(.25, .75)
  )

  pred_horizon <- sort(unique(.test$time[.test$status == 1]))

  pred_horizon <- pred_horizon[pred_horizon <= event_time_bounds['75%']]
  pred_horizon <- pred_horizon[pred_horizon >= event_time_bounds['25%']]

  if(is_empty(pred_horizon)) return(NULL)

  if(length(pred_horizon) > 30){
    pred_horizon <-
      pred_horizon[floor(seq(1, length(pred_horizon), length.out=30))]
  }

  # Don't pipe here - it can mess up the measurement
  # of time during model fitting and model predictions.

  model <- try(
    model_fit_fun(train = .train, pred_horizon = pred_horizon),
    silent = TRUE
  )

  predictions <- try(
    model_pred_fun(object = model,
                   test = .test,
                   pred_horizon = pred_horizon),
    silent = TRUE
  )

  sc <- try(
    Score(
      object = set_names(list(predictions$pred), model_type),
      formula = Surv(time, status) ~ 1,
      data = test,
      summary = c('IPA', 'ibs'),
      times = pred_horizon
    ),
    silent = TRUE
  )

  score <- tibble(model = model_type,
                  cstat = NA_real_,
                  ibs_scaled = NA_real_)

  if( !inherits(sc, 'try-error') ) {

    cstat <- sc$AUC$score |>
      group_by(model) |>
      summarize(cstat = mean(AUC))

    brier <- sc$Brier$score |>
      group_by(model) |>
      slice(n()) |>
      ungroup() |>
      summarize(
        ibs_scaled = (IBS[model=='Null model'] - IBS[model==model_type]) /
          IBS[model=='Null model']
      )

    score <- bind_cols(cstat, brier)

  }

  out <- score |>
    mutate(model = as.character(model),
           data = data_source,
           run = run_seed,
           .before = cstat)

  out$train_mean_time   <- mean(.train$time)
  out$train_mean_status <- mean(.train$status)

  if( !inherits(model, 'try-error') ) {
    out$time_fit <- model$time_fit
  }

  if( !inherits(predictions, 'try-error') ) {
    out$time_pred <- predictions$time
  }

  out$n_obs <- n_obs
  out$pred_corr_max <- pred_corr_max

  print(out)

  out

}
