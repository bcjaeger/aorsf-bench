#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param benchmark_vi_comb
bench_vi_summarize <- function(benchmark_vi_comb) {

  benchmark_vi_comb |>
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
