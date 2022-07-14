#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source
#' @param run_seed
bench_vi <- function(n_obs = 1000,
                     pred_corr_max = 0.3,
                     run_seed) {

  set.seed(run_seed)

  sim <- sim_surv(n_obs = n_obs, pred_corr_max = pred_corr_max)

  train <- sim$data

  vars_signal <- train %>%
    select(-time, -status, -starts_with('junk')) %>%
    names()

  vars_junk <- train %>%
    select(starts_with('junk')) %>%
    names()

  pred_horizon <- median(train$time)

  models <- set_names(
    c(
      'aorsf-negate',
      'aorsf-anova',
      # 'aorsf-shap',
      'aorsf-permute',
      'xgboost-shap',
      'xgboost-gain',
      'randomForestSRC-permutation'
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
    mutate(var_type = str_remove(vi_id, "_[0-9]+$")) |>
    group_by(name)

  vars_means <- vars_data %>%
    group_by(name) %>%
    mutate(vi = rescale(vi, to = c(0,1))) %>%
    group_by(name, var_type) %>%
    summarize(vi = mean(vi)) %>%
    pivot_wider(names_from = var_type, values_from = vi) %>%
    rename(model = name) %>%
    mutate(pred_corr_max = pred_corr_max,
           n_obs = n_obs)

  roc_grps <- map_dfr(
    .x = c("cmbn",
           "main",
           "nlin",
           "intr_main",
           "intr_hidden_cmbn",
           "intr_hidden_nlin"),
    .f = ~ vars_data |>
      filter(var_type %in% c('junk', .x)) |>
      roc_auc(vi, truth = vi_true, event_level = 'second') |>
      transmute(model = name,
                auc = .estimate,
                var_type = .x)
  )

  roc_overall <- vars_data |>
    group_by(name) |>
    roc_auc(vi, truth = vi_true, event_level = 'second') |>
    transmute(model = name,
              auc = .estimate,
              var_type = 'overall')

  roc <- bind_rows(
    roc_overall,
    roc_grps
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
    mutate(pred_corr_max = pred_corr_max,
           n_obs = n_obs)

  enframe(list(score = score, means = vars_means))


}
