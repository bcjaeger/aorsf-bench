#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
summarize_data_source <- function(analyses_real) {


  funs <- analyses_real |>
    distinct(data_source, data_load_fun) |>
    deframe()

  data_info <- map_dfr(
    .x = funs,
    .f = function(f){

      .x <- do.call(what = as.character(f), args = list())

      list(
        nrow = nrow(.x),
        ncol = ncol(.x),
        nevent = sum(.x$status),
        pcens = 1-mean(.x$status),
        pmiss = mean(is.na(.x)),
        pctns = mean(map_lgl(select(.x, -time, -status), is_double_ish))
      )
    },
    .id = 'data'
  )

  data_info |>
    mutate(
      label = recode(
        data,
        "accord_acm" = "ACCORD",
        "accord_cvd" = "ACCORD",
        "actg_death" = "ACTG 320",
        "actg_aids" = "ACTG 320",
        "breast" = "Early breast cancer",
        "colon_acm" = "Colon cancer",
        "colon_recur" = "Colon cancer",
        "flchain" = "Serum free light chain",
        "follic_death" = "FCL", # (Follicular Cell Lymphoma)
        "follic_relapse" = "FCL",
        "gbsg2" = "GBSG II", # German breast cancer study group
        "guide_it_hfhosp" = "GUIDE-IT",
        "guide_it_cvd" = "GUIDE-IT",
        "mgus2_death" = "Monoclonal gammopathy",
        "mgus2_pcm"   = "Monoclonal gammopathy",
        "nafld" = "Non-alcohol fatty liver disease",
        "pbc_orsf" = "Primary biliary cholangitis",
        "peakV02" = "Systolic Heart Failure",
        "phts" = "Heart Transplant",
        "rotterdam_acm" = "Rotterdam tumor bank",
        "rotterdam_recur" = "Rotterdam tumor bank",
        "sprint_acm" = "SPRINT",
        "sprint_cvd" = "SPRINT",
        "time_to_million" = "Movies released in 2015-2018",
        "vdv" = "Gene expression",
        "veteran" = "VA lung cancer trial"
      ),
      rpack = recode(
        data,
        "accord_acm" = NA_character_,
        "accord_cvd" = NA_character_,
        "actg_death" = "mlr3proba",
        "actg_aids"  = "mlr3proba",
        "breast" = "biospear",
        "colon_acm" = "survival",
        "colon_recur" = "survival",
        "flchain" = "survival",
        "follic_death" = "randomForestSRC",
        "follic_relapse" = "randomForestSRC",
        "gbsg2" = "TH.data",
        "guide_it_hfhosp" = NA_character_,
        "guide_it_cvd" = NA_character_,
        "mgus2_death" = "survival",
        "mgus2_pcm"   = "survival",
        "nafld" = "survival",
        "pbc_orsf" = "aorsf",
        "peakV02" = "randomForestSRC",
        "phts" = NA_character_,
        "rotterdam_acm" = "survival",
        "rotterdam_recur" = "survival",
        "sprint_acm" = NA_character_,
        "sprint_cvd" = NA_character_,
        "time_to_million" = "censored",
        "vdv" = "randomForestSRC",
        "veteran" = "randomForestSRC"
      ),
      cite = recode(
        data,
        "accord_acm" = "action2008effects",
        "accord_cvd" = "action2008effects",
        "actg_death" = "hosmer2002applied",
        "actg_aids"  = "hosmer2002applied",
        "actg" = "hosmer2002applied",
        "breast" = "desmedt2011multifactorial, hatzis2011genomic, ternes2017identification",
        "colon_acm" = "moertel1995fluorouracil",
        "colon_recur" = "moertel1995fluorouracil",
        "flchain" = "dispenzieri2012use, kyle2006prevalence",
        "follic_death" = "pintilie2006competing",
        "follic_relapse" = "pintilie2006competing",
        "gbsg2" = "schumacher1994rauschecker",
        "guide_it_hfhosp" = "felker2017effect",
        "guide_it_cvd" = "felker2017effect",
        "mgus2_death" = "kyle2002long",
        "mgus2_pcm"   = "kyle2002long",
        "nafld" = "allen2018nonalcoholic",
        "pbc_orsf" = "therneau2000cox",
        "peakV02" = "hsich2011identifying",
        "phts" = "wisotzkey2020risk",
        "rotterdam_acm" = "royston2013external",
        "rotterdam_recur" = "royston2013external",
        "sprint_acm" = "sprint2015randomized",
        "sprint_cvd" = "sprint2015randomized",
        "time_to_million" = "rpack0.0.0.9000censored",
        "vdv" = "van2002gene",
        "veteran" = "kalbfleisch2011statistical"
      ),
      outcome = recode(
        data,
        "accord_acm" = "death",
        "accord_cvd" = "CVD death",
        "actg_death" = "death",
        "actg_aids"  = "AIDS diagnosis",
        "breast" = "recurrence or death",
        "colon_acm" = "death",
        "colon_recur" = "recurrence",
        "flchain" = "death",
        "follic_death" = "death",
        "follic_relapse" = "relapse",
        "gbsg2" = "recurrence or death",
        "guide_it_hfhosp" = "HF hospitalization",
        "guide_it_cvd" = "CVD death",
        "mgus2_death" = "death",
        "mgus2_pcm"   = "malignancy",
        "nafld" = "death",
        "pbc_orsf" = "death",
        "peakV02" = "death",
        "phts" = "graft-loss or death",
        "rotterdam_acm" = "death",
        "rotterdam_recur" = "recurrence",
        "sprint_acm" = "death",
        "sprint_cvd" = "CVD death",
        "time_to_million" = "gross 1M USD",
        "vdv" = "breast cancer",
        "veteran" = "death"
      ),
      .before = 2
    )

}

is_double_ish <- function(x){

  is.numeric(x) & length(unique(na.omit(x))) > 12

}


# data_info |>
#   mutate(
#     label = recode(
#       data,
#       vdv = "Van't Veer LJ (2002); breast cancer",
#       veteran = "Kalbfleisch JD (1980); lung cancer",
#       colon = "Laurie JA (1989); colon cancer",
#       pbc_orsf = "Therneau T (2000); Primary biliary cholangitis",
#       time_to_million = "Hvitfeldt E (2022), Movie grosses 1M USD",
#       gbsg2 = "Schumacher M (1994); breast cancer",
#       peakV02 = "Hsich E (2011); Systolic heart failure",
#       flchain = "Dispenzieri A (2012); serum free light chain",
#       nafld = "Allen AM (2018); Non-alcohol fatty liver disease",
#       rotterdam = "Royston P (2013); breast cancer",
#       actg = "Hosmer DW (2008); ACTG 320",
#       guide_it = "Felker GM (2017); GUIDE-IT",
#       breast = "Desmedt C (2011); breast cancer",
#       sprint_cvd = "Jaeger BC (2022); cardiovascular mortality",
#       sprint_acm = "Jaeger BC (2022); all-cause mortality"
#     ),
#     tbl_label = glue("{label}; n = {nrow}, p = {ncol}")
#   )

