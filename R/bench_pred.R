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
                       model_prd_fun,
                       n_obs = NULL,
                       n_z = NULL,
                       correlated_x = NULL,
                       run_seed,
                       test_prop = 1/4) {

  conflict_prefer("filter", "dplyr")
  conflict_prefer("slice", "dplyr")
  conflict_prefer("summarize", "dplyr")
  conflict_prefer("Predict", "modeltools")

  set.seed(run_seed)

  data_all <- data_load_fun(n_obs = n_obs,
                            n_z = n_z,
                            correlated_x = correlated_x)

  if(data_source == 'sim'){

    train <- data_all

    test <- sim_surv(n_obs = 5000,
                     n_z = n_z,
                     correlated_x = correlated_x) |>
      getElement('data')

  } else {

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

  pred_horizon <- median(.train$time)

  model <- model_fit_fun(train = .train) |>
    model_prd_fun(test = .test, pred_horizon = pred_horizon)

  sc <- try(
    Score(
      object = set_names(list(model$pred), model_type),
      formula = Surv(time, status) ~ 1,
      data = test,
      summary = 'IPA',
      times = pred_horizon
    ),
    silent = TRUE)

  score <- tibble(model = model_type,
                  cstat = NA_real_,
                  Brier = NA_real_,
                  IPA = NA_real_)

  if( !inherits(sc, 'try-error') ) {

    cstat <- sc$AUC$score |>
      select(model, cstat = AUC)

    brier <- sc$Brier$score |>
      select(model, Brier, IPA)

    score <- cstat |>
      left_join(brier, by = 'model') |>
      as_tibble()

  }

  out <- score |>
    mutate(data = data_source,
           run = run_seed,
           .before = 1)

  out$pred_horizon <- pred_horizon
  out$time_fit     <- model$time_fit
  out$time_pred    <- model$time_pred
  out$n_obs        <- n_obs %||% NA_integer_
  out$n_z          <- n_z %||% NA_integer_
  out$correlated_x <- correlated_x %||% NA_real_
  print(out)
  out

}


