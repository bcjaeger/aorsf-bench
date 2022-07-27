#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_pred_comb
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
