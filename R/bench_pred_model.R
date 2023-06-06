#' @description fits bayesian linear mixed models to the benchmark
#'   results to conduct inference on model performance.
#'
#' @param bm_pred_clean a target with cleaned benchmark results.
#' @param data_key a target with information about data sets.
#' @param model_key a target with information about modeling algos.
#'
bench_pred_model <- function(bm_pred_clean, data_key, model_key) {

  bm_pred_mdat <- bm_pred_clean |>
    select(data, run, model, cstat, ibs_scaled) |>
    pivot_wider(names_from = model,
                values_from = c(cstat, ibs_scaled),
                names_sep = '..') |>
    mutate(
      across(
        .cols = starts_with('cstat'),
        .fns = ~ .x - cstat..aorsf_fast
      ),
      across(
        .cols = starts_with('ibs_scaled'),
        .fns = ~ .x - ibs_scaled..aorsf_fast
      )
    ) |>
    select(-ibs_scaled..aorsf_fast, -cstat..aorsf_fast) |>
    pivot_longer(cols = matches('^cstat|^ibs_scaled')) |>
    separate(name, into = c("metric", "model"), sep = '\\.\\.') |>
    mutate(across(where(is.character), as.factor))

  mdl <- list(

    ibs_scaled = stan_glmer(
      data = filter(bm_pred_mdat, model != 'xgb_aft'),
      formula = value ~ -1 + model + (1 | data/run),
      subset = metric == 'ibs_scaled'
    ),

    cstat = stan_glmer(
      data = bm_pred_mdat,
      formula = value ~ -1 + model + (1 | data/run),
      subset = metric == 'cstat'
    )

  )

  newdata_cstat <- distinct(bm_pred_mdat, model)

  newdata_ibs_scaled <- newdata_cstat %>%
    filter(model != 'xgb_aft') %>%
    droplevels()

  data_infer <- map2_dfr(
    .x = mdl,
    .y = list(newdata_ibs_scaled,
              newdata_cstat),
    .id = 'metric',
    .f = ~ posterior_epred(.x, newdata = .y,  re.form = ~0) |>
      as_tibble() |>
      set_names(.y$model) |>
      pivot_longer(everything(), names_to = 'model') |>
      add_row(model = 'aorsf_fast', value = Inf) |>
      mutate(model = factor(model),
             model = fct_reorder(model, .x = value, .desc = TRUE))
  )

  list(model = mdl, posterior = data_infer)

}
