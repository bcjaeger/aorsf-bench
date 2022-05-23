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

  x = c(
    "vdv",
    "veteran",
    "colon",
    "pbc_orsf",
    'time-to-million',
    'gbsg2',
    'peakV02',
    'flchain',
    'nafld',
    "rotterdam",
    "actg",
    "guide_it",
    "breast",
    "sprint-cvd",
    "sprint-acm"
  ) |>
    set_names()

  all_data <- map(.x = x, .f = load_data)

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



