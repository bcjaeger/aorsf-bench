#' @description
#'  creates a summary dataset from the variable importance benchmark results.
#'
#' @param bm_vi_comb combined results from benchmark of variable importance.
#'
bench_vi_summarize <- function(bm_vi_comb) {

  bm_vi_comb %>%
    as.data.table() %>%
    .[name == 'score'] %>%
    dt_unnest(col = value) %>%
    group_by(model, pred_corr_max, n_obs) |>
    summarize(
      across(c(overall, cmbn, main, nlin, starts_with("intr"), time),
             .fns = ~mean(.x, na.rm = TRUE)),
      .groups = 'drop') |>
    arrange(pred_corr_max, n_obs, desc(overall)) |>
    group_by(pred_corr_max, n_obs) |>
    mutate(
      overall_prop = overall[1] / overall,
      .after = 'overall'
    )

}


