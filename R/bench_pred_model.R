#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_pred_clean
#' @param data_key
#' @param model_key
bench_pred_model <- function(bm_pred_clean, data_key, model_key) {

  # fit <- stan_glmer(data = bm_pred_clean,
  #                   formula = ibs_scaled ~ model + (1|data/run))

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


# data_gg <- split(data_infer$ibs_scaled$value,
#                  data_infer$ibs_scaled$model)
#
# mdl_levels <- levels(data_infer$ibs_scaled$model)
#
# inline_plot <-
#   tibble(
#     model = recode(mdl_levels, !!!deframe(model_key)),
#     box = ""
#   )
#
# inline_plot |>
#   kbl(booktabs = TRUE) |>
#   kable_paper(full_width=TRUE) |>
#   column_spec(2, image = spec_boxplot(data_gg, width = 1000))
