#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source
#' @param run_seed
bench_vi <- function(data_source,
                     run_seed,
                     correlated_x,
                     n_obs,
                     n_z) {

  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("summarize", "dplyr")

  set.seed(run_seed)

  switch(

    data_source,

    'sim' = {

      sim <- sim_surv(n_obs = n_obs, n_z = n_z, correlated_x = correlated_x)

      train <- sim$data
      vars_signal <- sim$vars_signal
      vars_junk <- sim$vars_junk

    }

  )

  pred_horizon <- median(train$time)

  models <- set_names(
    c(
      'aorsf',
      'aorsf_menze',
      'xgboost',
      'randomForestSRC',
      'ranger'
    )
  )

  vars <- map(models,
              .f = model_varsel,
              train = as.data.frame(train))

  vars_truth <-
    tibble(vi_id = c(vars_signal, vars_junk)) |>
    mutate(vi_true = if_else(vi_id %in% vars_signal, 1, 0),
           vi_true = factor(vi_true))

  vars_data <- enframe(vars) |>
    unnest_wider(col = value) |>
    unnest_longer(col = vi) |>
    select(name, vi, vi_id) |>
    left_join(vars_truth) |>
    mutate(var_type = str_sub(vi_id, start = 0, end = 1)) |>
    group_by(name)

  roc_x <- vars_data |>
    filter(var_type %in% c('z', 'x')) |>
    roc_auc(vi, truth = vi_true, event_level = 'second') |>
    transmute(model = name,
              auc = .estimate,
              var_type = 'x')

  roc_g <- vars_data |>
    filter(var_type %in% c('z', 'g')) |>
    roc_auc(vi, truth = vi_true, event_level = 'second') |>
    transmute(model = name,
              auc = .estimate,
              var_type = 'g')

  roc_w <- vars_data |>
    filter(var_type %in% c('z', 'w')) |>
    roc_auc(vi, truth = vi_true, event_level = 'second') |>
    transmute(model = name,
              auc = .estimate,
              var_type = 'w')

  roc_v <- vars_data |>
    filter(var_type %in% c('z', 'v')) |>
    roc_auc(vi, truth = vi_true, event_level = 'second') |>
    transmute(model = name,
              auc = .estimate,
              var_type = 'v')

  # roc_c <- vars_data |>
  #   filter(var_type %in% c('z', 'c')) |>
  #   roc_auc(vi, truth = vi_true, event_level = 'second') |>
  #   transmute(model = name,
  #             auc = .estimate,
  #             var_type = 'c')

  roc_overall <- vars_data |>
    group_by(name) |>
    roc_auc(vi, truth = vi_true, event_level = 'second') |>
    transmute(model = name,
              auc = .estimate,
              var_type = 'overall')

  roc <- bind_rows(
    roc_overall,
    roc_x,
    roc_w,
    roc_g,
    roc_v
    # roc_c
  ) |>
    pivot_wider(names_from = var_type,
                values_from = auc)

  times <-
    map_dfr(vars, 'time') |>
    pivot_longer(cols = any_of(models),
                 names_to = 'model',
                 values_to = 'time')

  score <- roc |>
    left_join(times) |>
    as_tibble() |>
    arrange(desc(overall)) |>
    mutate(xcorr = correlated_x,
           n_obs = n_obs,
           n_z = n_z)

  print(score)

  score

}
