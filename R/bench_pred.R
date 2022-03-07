#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source
#' @param run_seed
bench_pred <- function(data_source, run_seed, test_prop = 1/2) {

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

    'sim' = {

      data_all <- sim_surv() |>
        getElement('data')

    }

  )

  pred_horizon <- median(data_all$time)

  set.seed(run_seed)

  test_index <- sample(x = seq(nrow(data_all)),
                       size = round(nrow(data_all) * test_prop),
                       replace = FALSE)

  train <- data_all[-test_index, ]
  test <- data_all[test_index, ]

  models <- set_names(
    c(
      'aorsf',
      # 'obliqueRSF',
      'xgboost',
      'randomForestSRC',
      'ranger'
    )
  )

  fits <- map(models, model_fit, train = as.data.frame(train))

  prds <- map2(.x = fits,
               .y = models,
               .f = model_pred,
               test = as.data.frame(test),
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
    as_tibble()

  print(score)

  score

}


