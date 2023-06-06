
#' Visualize the results from benchmark of variable importance
#'
#' @param bm_vi_comb the combined variable importance benchmark results
#'

bench_vi_mean_visualize <- function(bm_vi_comb) {

  vars_vi <- c('main', 'cmbn', 'nlin', 'intr', 'junk')
  vars_by <- c('model', 'pred_corr_max', 'n_obs')

  models <-
    c(
      "aorsf-negate",
      "aorsf-anova",
      "aorsf-permute",
      "xgboost-shap",
      "xgboost-gain",
      "randomForestSRC-permutation"
    )

  colors <- c("purple", RColorBrewer::brewer.pal(n = 5, name = 'Oranges'))

  bm_vi_means <- bm_vi_comb %>%
    as.data.table() %>%
    .[name == 'means'] %>%
    dt_unnest(col = value) %>%
    .[, intr := (intr_hidden_cmbn + intr_hidden_nlin + intr_main) / 3] %>%
    select(all_of(c(vars_by, vars_vi))) %>%
    .[,
      (vars_vi) := lapply(.SD, function(x) x - junk),
      .SDcols = vars_vi] %>%
    .[, lapply(.SD, median, na.rm = T), by = vars_by] %>%
    dt_pivot_longer(cols = -c(model, pred_corr_max, n_obs)) %>%
    .[, name := factor(name, levels = vars_vi)] %>%
    .[, model := factor(model, levels = models)]

  ggplot(bm_vi_means, aes(x = name, y = value, color = model)) +
    geom_point() +
    coord_flip() +
    facet_grid(pred_corr_max ~ n_obs)




}
