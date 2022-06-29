#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

make_model_key <- function() {

  tibble(
    term = c(
      "aorsf_fast",
      "aorsf_cph",
      "aorsf_random",
      "aorsf_net",
      "obliqueRSF",
      "rsfse",
      "rotsf",
      "cif",
      "cox_net",
      "coxtime",
      "xgb_cox",
      "xgb_aft",
      "randomForestSRC",
      "ranger"
    ),
    label = c(
      "aorsf-fast",
      "aorsf-cph",
      "aorsf-random",
      "aorsf-net",
      "obliqueRSF-net",
      "cif-extension",
      "cif-rotate",
      "cif-standard",
      "glmnet-cox",
      "nn-cox",
      "xgboost-cox",
      "xgboost-aft",
      "rsf-standard",
      "ranger-extratrees"
    ),
  )

}
