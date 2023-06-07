#' @description
#'   this is sort of an alpha version of a function that attempts to
#'   run a variable selection step prior to fitting a model and then
#'   validating it in external data. It isn't included in my main analysis
#'   but I keep it here in case it would be useful to flesh it out in
#'   other projects that are more focused on variable selection.
#'
#'
#' @param source the data to be used for the experiment. Valid options
#'   are 'pbc_orsf', 'rotterdam', and 'sim'.
#' @param test_prop proportion of data to be held out for testing.
#' @param keep_prop proportion of predictors to keep in selection step.
#' @param run_seed random seed value.
#'
varsel_run <- function(source = source,
                       run_seed = run_seed,
                       test_prop = 1/2,
                       keep_prop = 1/2) {

  switch(

    source,

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

    'sim' = {

      data_all <- sim_surv() |>
        rename(time = eventtime)

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
      'randomForestSRC',
      'ranger'
    )
  )

  vis <- map(models,
             .f = model_varsel,
             train = as.data.frame(train))

  vis$aorsf$vi

  fits <- map2(
    models,
    vars,
    .f = ~model_fit(.x, train = as.data.frame(train[, .y$vars]))
  )

  prds <- map2(.x = fits,
               .y = models,
               .f = model_pred,
               test = as.data.frame(test),
               pred_horizon = pred_horizon)

  times_sel <- vars |>
    map_dfr('time') |>
    mutate(action = 'var_sel')

  times_fit <- fits |>
    map_dfr('time') |>
    mutate(action = 'fit')

  times_prd <- prds |>
    map_dfr('time') |>
    mutate(action = 'prd')

  times <-
    bind_rows(times_sel, times_fit, times_prd) |>
    mutate(data = source, run = run_seed) |>
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
