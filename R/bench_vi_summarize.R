#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_vi_comb
#' @param bm_pred_comb
bench_vi_summarize <- function(bm_vi_comb) {

  bm_vi_comb |>
    group_by(model, pred_corr_max, n_obs) |>
    summarize(across(everything(), ~mean(.x)), .groups = 'drop') |>
    arrange(pred_corr_max, n_obs, desc(overall)) |>
    group_by(pred_corr_max, n_obs) |>
    mutate(
      overall_prop = overall[1] / overall,
      .after = 'overall'
    )

}

# bm_vi_smry <- bm_vi_comb |>
#   group_by(model) |>
#   summarize(across(everything(), ~mean(.x)), .groups = 'drop') |>
#   arrange(pred_corr_max, n_obs, desc(overall)) |>
#   group_by(pred_corr_max, n_obs) |>
#   mutate(
#     overall_prop = overall[1] / overall,
#     .after = 'overall'
#   )
#
# gg_data <- select(bm_vi_smry, -overall_prop, -time) %>%
#   mutate(overall = 2 * (overall - .50))
#
# ggplot(gg_data) +
#   aes(x = reorder(model, overall), fill = reorder(model, overall), y = overall) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   coord_flip() +
#   theme(legend.position = '') +
#   labs(y = '2 * (C-statistic - 1/2)',
#        x = '')
