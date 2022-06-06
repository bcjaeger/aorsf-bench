#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

make_model_key <- function() {

  tibble(
    term = c(
      "aorsf_cph_1",
      "aorsf_cph_15",
      "aorsf_random",
      "aorsf_net",
      "obliqueRSF",
      "cif",
      "cox_net",
      "coxtime",
      "xgb_cox",
      "xgb_aft",
      "randomForestSRC",
      "ranger"
    ),
    label = c(
      "aorsf-cph(maxit = 1)",
      "aorsf-cph(maxit = 15)",
      "aorsf-random",
      "aorsf-net",
      "obliqueRSF-net",
      "party-cif",
      "glmnet-cox",
      "nn-cox",
      "xgboost-cox",
      "xgboost-aft",
      "rfsrc-standard",
      "ranger-extratrees"
    ),
  )

}
