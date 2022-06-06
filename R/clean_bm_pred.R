#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_pred_comb
clean_bm_pred <- function(bm_pred_comb) {

  to_omit <- bm_pred_comb |>
    filter(is.na(cstat) | is.na(ibs_scaled)) |>
    distinct(data, run) |>
    group_by(data) |>
    group_split() |>
    map(as.list)

  for(i in seq_along(to_omit)){

    bm_pred_comb <- bm_pred_comb |>
      filter( ! (data %in% unique(to_omit[[i]]$data) &
                   run %in% to_omit[[i]]$run) )

  }

  # bm_pred_comb$IPA[bm_pred_comb$model == 'xgb_aft'] <- NA_real_

  bm_pred_comb |>
    mutate(
      across(starts_with("time_"),
             as.numeric)
    )

}
