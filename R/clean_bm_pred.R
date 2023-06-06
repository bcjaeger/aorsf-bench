
#' Clean results from benchmark of prediction accuracy
#'
#' @param bm_pred_real_comb combined results from benchmark on real data
#' @param bm_pred_sim_comb combined results from benchmark on simulated data
#'
#' @details
#'  experiments where at least one model did not obtain a valid fit
#'  are omitted. This is very infrequent and every instance where it
#'  occurs seems to be due to the neural network model having an error
#'  during the fitting procedure. It may be something to do with
#'  smaller data causing issues in the backward propagation algo. Either
#'  way, it is not an aorsf problem.
#'
#'  Note that bm_pred_sim_comb's default value is NULL. If you include
#'  this target, it will make the output contain results from the benchmark
#'  of prediction accuracy of both real data and simulated data. I toyed
#'  with the idea of combining the real data with simulated data to help make
#'  the overall comparison of learning algos more comprehensive. I decided
#'  not to combine the two data types in the end because I thought
#'  a benchmark with exclusively real data was more informative than a
#'  benchmark that included data I created.
#'

clean_bm_pred <- function(bm_pred_real_comb,
                          bm_pred_sim_comb = NULL) {

  bm_pred_comb <- bm_pred_real_comb

  if(!is.null(bm_pred_sim_comb)){

    bm_pred_sim_comb <- bm_pred_sim_comb %>%
      mutate(data = paste(data, n_obs, pred_corr_max*100, sep = '_'))

    bm_pred_comb <- bind_rows(bm_pred_comb, bm_pred_sim_comb)

  }

  to_omit <- bm_pred_comb |>
    filter(
      is.na(cstat) |
        is.na(ibs_scaled) |
        is.infinite(ibs_scaled) |
        is.infinite(cstat)
    ) |>
    distinct(model, data, run) |>
    group_by(data) |>
    group_split() |>
    map(as.list)

  for(i in seq_along(to_omit)){

    bm_pred_comb <- bm_pred_comb |>
      filter( ! (data %in% unique(to_omit[[i]]$data) &
                   run %in% to_omit[[i]]$run) )

  }

  # bm_pred_comb$IPA[bm_pred_comb$model == 'xgb_aft'] <- NA_real_

  data_omit <- map_dfr(to_omit, as_tibble)

  data_out <- bm_pred_comb |>
    mutate(
      across(starts_with("time_"),
             as.numeric)
    )

  list(data = data_out,
       omit = data_omit)



}
