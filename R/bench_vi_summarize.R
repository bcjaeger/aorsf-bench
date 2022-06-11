#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_vi_comb
#' @param bm_pred_comb
bench_vi_summarize <- function(bm_vi_comb) {

  bm_vi_comb |>
    group_by(model, xcorr, n_obs, n_z) |>
    summarize(across(everything(), ~mean(.x))) |>
    arrange(xcorr, n_obs, n_z, desc(overall)) |>
    group_by(xcorr, n_obs, n_z) |>
    mutate(
      overall_prop = overall[1] / overall,
      .after = 'overall'
    )

}
