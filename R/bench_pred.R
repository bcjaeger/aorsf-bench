#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source
#' @param run_seed
bench_pred <- function(data_source,
                       n_obs = NULL,
                       n_z = NULL,
                       correlated_x = NULL,
                       run_seed,
                       test_prop = 1/4) {

  conflict_prefer("filter", "dplyr")
  conflict_prefer("slice", "dplyr")
  conflict_prefer("summarize", "dplyr")
  conflict_prefer("Predict", "modeltools")


  # one core per worker
  options(parallelly.availableCores.custom = function() { 1L })

  set.seed(run_seed)

  data_all <- load_data(data_source,
                        n_obs = n_obs,
                        n_z = n_z,
                        correlated_x = correlated_x)

  pred_horizon <- median(data_all$time)


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


  models <- set_names(
    c(
      'aorsf-1',
      'aorsf-15',
      'aorsf-net',
      'cif',
      'coxtime',
      'obliqueRSF',
      'xgboost',
      'randomForestSRC',
      'ranger'
    )
  )


  imputer <- recipe(x = train, time + status ~ .) |>
    step_impute_mean(all_numeric_predictors()) |>
    step_impute_mode(all_nominal_predictors()) |>
    step_nzv(all_predictors()) |>
    step_range(all_numeric_predictors()) |>
    prep()

  .train <- juice(imputer)
  .test <- bake(imputer, new_data = test)


  fits <- map(models, model_fit, train = as.data.frame(.train))

  prds <- map2(.x = fits,
               .y = models,
               .f = model_pred,
               test = as.data.frame(.test),
               pred_horizon = pred_horizon)

  times_fit <- fits |>
    map_dfr('time') |>
    mutate(action = 'fit')

  times_prd <- prds |>
    map_dfr('time') |>
    mutate(action = 'prd')

  times <-
    bind_rows(times_fit, times_prd) |>
    mutate(data = data_source, run = run_seed) |>
    pivot_longer(cols = any_of(models),
                 names_to = 'model',
                 values_to = 'time') |>
    pivot_wider(names_from = action,
                values_from = time,
                names_prefix = 'time_')

  sc <- try(Score(
    object = map(prds, 'prediction'),
    formula = Surv(time, status) ~ 1,
    data = test,
    summary = 'IPA',
    times = pred_horizon
  ), silent = TRUE)

  if(inherits(sc, 'try-error')) return(NULL)

  cstat <- sc$AUC$score |>
    select(model, cstat = AUC)

  brier <- sc$Brier$score |>
    select(model, Brier, IPA)

  score <- cstat |>
    left_join(brier, by = 'model') |>
    left_join(times, by = 'model') |>
    as_tibble() |>
    mutate(n_obs = n_obs,
           n_z = n_z,
           correlated_x = correlated_x)

  score |>
    arrange(desc(cstat)) |>
    select(-Brier, -run, -time_prd, -n_z, -n_obs) |>
    print(n=100)

  score

}


