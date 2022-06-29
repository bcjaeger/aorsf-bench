#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source
#' @param run_seed
bench_pred <- function(data_source,
                       model_type,
                       data_load_fun,
                       model_fit_fun,
                       model_pred_fun,
                       n_obs = NULL,
                       pred_corr_max = NULL,
                       run_seed,
                       test_prop = 1/2) {

  set.seed(run_seed)

  data_all <- data_load_fun(n_obs = n_obs,
                            pred_corr_max = pred_corr_max)


  if(data_source == 'sim'){

    data_all <- data_all$data

    train <- data_all

    test <- sim_surv(n_obs = 5000,
                     pred_corr_max = pred_corr_max) |>
      getElement('data')

  } else {

    # Some R packages (not aorsf) have trouble
    # with factors that have special characters
    data_all <-  data_all |>
      mutate(across(where(is.character), as.factor),
             across(where(is.factor), simplify_levels))

    test_index <- sample(x = seq(nrow(data_all)),
                         size = round(nrow(data_all) * test_prop),
                         replace = FALSE)

    train <- data_all[-test_index, ]
    test <- data_all[test_index, ]


  }

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

  if(data_source == 'sim'){
    out$n_obs <- n_obs
    out$pred_corr_max <- pred_corr_max
  }

  print(out)
  out

}


