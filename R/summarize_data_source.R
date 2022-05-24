#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
summarize_data_source <- function(x) {

  conflict_prefer("filter", "dplyr")
  conflict_prefer("slice", "dplyr")
  conflict_prefer("summarize", "dplyr")
  conflict_prefer("Predict", "modeltools")

  all_data = list(
    "vdv" = load_vdv(),
    "veteran" = load_veteran(),
    "colon" = load_colon(),
    "pbc_orsf" = load_pbc_orsf(),
    'time_to_million' = load_time_to_million(),
    'gbsg2' = load_gbsg2(),
    'peakV02' = load_peakV02(),
    'flchain' = load_flchain(),
    'nafld' = load_nafld(),
    "rotterdam" = load_rotterdam(),
    "actg" = load_actg(),
    "guide_it" = load_guide_it(),
    "breast" = load_breast(),
    "sprint_cvd" = load_sprint_cvd(),
    "sprint_acm" = load_sprint_acm()
  )

  data_info <- map_dfr(
    .x = all_data,
    .f = ~ list(
      nrow = nrow(.x),
      ncol = ncol(.x),
      nevent = sum(.x$status),
      pcens = 1-mean(.x$status),
      pmiss = mean(is.na(.x)),
      pctns = mean(map_lgl(select(.x, -time, -status), is_double_ish))
    ),
    .id = 'data'
  )

  data_info


}

is_double_ish <- function(x){

  is.numeric(x) & length(unique(na.omit(x))) > 12

}



