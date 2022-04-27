#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_vi_comb
#' @param bm_pred_comb
bench_vi_summarize <- function(bm_pred_comb, bm_vi_comb) {


  bm_pred_comb |>
    mutate(
      data = if_else(
        data == 'sim',
        paste(data, n_obs, n_z, correlated_x, sep = '_'),
        data
      )
    ) |>
    group_by(model, data) |>
    summarize(across(.cols = c(cstat, IPA, time_fit, time_prd),
                     .fns = median)) |>
    arrange(data, desc(cstat))

  group_by(model, data, )

  bm_vi_comb |>
    group_by(model, xcorr, n_obs, n_z) |>
    summarize(across(everything(), ~mean(.x))) |>
    arrange(xcorr, n_obs, n_z, desc(overall)) |>
    group_by(xcorr, n_obs, n_z) |>
    mutate(
      overall_disc = (overall - .5) * 2,
      overall_prop = overall_disc / overall_disc[1],
      .after = 'overall'
    )

}
