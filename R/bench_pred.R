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
  conflict_prefer("summarize", "dplyr")
  conflict_prefer("Predict", "modeltools")

  # one core per worker
  options(parallelly.availableCores.custom = function() { 1L })

  print(paste("Number of cores available:", availableCores()))

  set.seed(run_seed)

  switch(

    data_source,

    'pbc_orsf' = {
      data_all <- aorsf::pbc_orsf |>
        select(-id) |>
        as_tibble()
    },

    'rotterdam' = {
      data_all <- survival::rotterdam |>
        mutate(time = pmin(rtime, dtime),
               status = if_else(rtime < dtime, recur, death)) |>
        select(age, meno, size, grade, nodes, pgr, er, hormon,
               time, status) |>
        as_tibble()
    },

    'actg' = {

      data_all <- mlr3proba::actg |>
        as_tibble() |>
        select(-id, -time_d, -censor_d) |>
        rename(status = censor)

    },

    'guide_it' = {

      data_all <- guide_it_build() |>
        select(-deidnum)

    },

    'breast' = {

      data("Breast",package='biospear')

      data_all <- Breast

      names(data_all)[4:ncol(data_all)] <-
        paste('var',4:ncol(data_all),sep='_')

    },

    'sim' = {

      data_all <- sim_surv(n_obs = n_obs,
                           n_z = n_z,
                           correlated_x = correlated_x) |>
        getElement('data')

    },

    'sprint-cvd' = {

      data_all <- load_sprint_ndi(outcome = 'cvd')

    },

    'sprint-acm' = {

      data_all <- load_sprint_ndi(outcome = 'acm')

    }

  )

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
      'aorsf-3',
      'aorsf-5',
      'aorsf-10',
      'aorsf-net',
      'cif',
      'coxtime',
      # 'obliqueRSF',
      'xgboost',
      'randomForestSRC',
      'ranger'
    )
  )


  not_all_missing <- function(x){ !all(is.na(x)) & !all() }

  imputer <- recipe(x = train, time + status ~ .) |>
    step_impute_mean(all_numeric_predictors()) |>
    step_impute_mode(all_nominal_predictors()) |>
    step_nzv() |>
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

  sc <- Score(
    object = map(prds, 'prediction'),
    formula = Surv(time, status) ~ 1,
    data = test,
    summary = 'IPA',
    times = pred_horizon
  )

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

  print(score)

  score

}


