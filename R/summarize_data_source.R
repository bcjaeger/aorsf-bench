#' @description
#'   this function is used to crate a target that in turn contains
#'   relevant information about our datasets. It is very helpful to
#'   keep this function and use it in papers/posters/presentations
#'   to ensure that our descriptions of the data is consistent in each
#'   deliverable and we don't have to update multiple outputs whenever
#'   we update the data descriptions.
#'
#' @param analyses_real the analyses that are selected for running our
#'   benchmark on real data. This is a dataset that is created at the
#'   beginning of _targets.R
#'
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
        "nki" = "NKI 70 gene signature",
        "lung" = "Lung cancer",
        "lung_ncctg" = "NCCTG Lung Cancer",
        "time_to_million" = "Movies released in 2015-2018",
        "vdv" = "Gene expression",
        "veteran" = "VA lung cancer trial",
        "mesa_hf" = "MESA",
        "mesa_chd" = "MESA",
        "mesa_stroke" = "MESA",
        "mesa_death" = "MESA",
        "aric_hf" = "ARIC",
        "aric_chd" = "ARIC",
        "aric_stroke" = "ARIC",
        "aric_death" = "ARIC",
        "jhs_stroke" = "JHS",
        "jhs_chd" = "JHS"
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
        "nki" = "OpenML",
        "lung" = "OpenML",
        "lung_ncctg" = "survival",
        "time_to_million" = "censored",
        "vdv" = "randomForestSRC",
        "veteran" = "randomForestSRC",
        "mesa_hf" = NA_character_,
        "mesa_chd" = NA_character_,
        "mesa_stroke" = NA_character_,
        "mesa_death" = NA_character_,
        "aric_hf" = NA_character_,
        "aric_chd" = NA_character_,
        "aric_stroke" = NA_character_,
        "aric_death" = NA_character_,
        "jhs_stroke" = NA_character_,
        "jhs_chd" = NA_character_
      ),
      preproc = recode(
        data,
        "accord_acm" = "baseline data including biomarkers, questionnaires, and medications were included as predictors. Outcomes occuring the same day as the baseline examination were given a time value of 1/2 day rather than 0 days",
        "accord_cvd" = "baseline data including biomarkers, questionnaires, and medications were included as predictors. Outcomes occuring the same day as the baseline examination were given a time value of 1/2 day rather than 0 days",
        "actg_death" = "redundant predictors were dropped",
        "actg_aids"  = "redundant predictors were dropped",
        "breast" = "none",
        "colon_acm" = "the predictor, node4, which is an indicator for having more than 4 positive lymph nodes, was dropped, while the nodes predictor, which indicates the number of positive lymph nodes, was retained",
        "colon_recur" = "the predictor, node4, which is an indicator for having more than 4 positive lymph nodes, was dropped, while the nodes predictor, which indicates the number of positive lymph nodes, was retained",
        "flchain" = "the chapter variable, which indicates death status, was removed, since death was the outcome. Outcomes occurring on day 0 were assumed to have a time of 1/2 day rather than 0 days",
        "follic_death" = "none",
        "follic_relapse" = "none",
        "gbsg2" = "none",
        "guide_it_hfhosp" = "baseline data including biomarkers, questionnaires, and randomized group were included as predictors",
        "guide_it_cvd" = "baseline data including biomarkers, questionnaires, and randomized group were included as predictors",
        "mgus2_death" = "none",
        "mgus2_pcm"   = "none",
        "nafld" = "data from before or on the index data were used as predictors. The mean value prior to the index date for lab values in nafld2 was used as a predictor, and the number of days between the most recent lab test and the index date was also used as a predictor",
        "pbc_orsf" = "none",
        "peakV02" = "none",
        "phts" = "none",
        "rotterdam_acm" = "none",
        "rotterdam_recur" = "none",
        "sprint_acm" = "baseline data including biomarkers, cognitive questionnaires, medications, and randomization group were included as predictors. Predictors with over 40 percent missing data were dropped. Zero variance predictors were also dropped",
        "sprint_cvd" = "baseline data including biomarkers, cognitive questionnaires, medications, and randomization group were included as predictors. Predictors with over 40 percent missing data were dropped. Zero variance predictors were also dropped",
        "nki" = "none",
        "lung" = "status was transformed to have values of 0 and 1 instead of 1 and 2",
        "lung_ncctg" = "institution code was not used as a predictor, and values of both sex and event status were transformed to be 0 and 1 instead of 1 and 2",
        "time_to_million" = "text and date variables (movie title, release date, and distributor) were dropped.",
        "vdv" = "none",
        "veteran" = "none",
        "mesa_hf" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "mesa_chd" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "mesa_stroke" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "mesa_death" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "aric_hf" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "aric_chd" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "aric_stroke" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "aric_death" = "visit 1 data including biomarkers, health behaviors, and comorbidities were included as predictors",
        "jhs_stroke" = "visit 1 data including biomarkers, health behaviors, neighborhood characteristics, and comorbidities were included as predictors",
        "jhs_chd" = "visit 1 data including biomarkers, health behaviors, neighborhood characteristics, and comorbidities were included as predictors"
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
        "nki" = "van2002gene",
        "lung" = "director2008gene",
        "lung_ncctg" = "loprinzi1994prospective",
        "time_to_million" = "censored",
        "vdv" = "van2002gene",
        "veteran" = "kalbfleisch2011statistical",
        "mesa_hf" = "bild2002multi",
        "mesa_chd" = "bild2002multi",
        "mesa_stroke" = "bild2002multi",
        "mesa_death" = "bild2002multi",
        "aric_hf" = "aric1989atherosclerosis",
        "aric_chd" = "aric1989atherosclerosis",
        "aric_stroke" = "aric1989atherosclerosis",
        "aric_death" = "aric1989atherosclerosis",
        "jhs_stroke" = "taylor2005toward",
        "jhs_chd" = "taylor2005toward"
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
        "nki" = "death or metastasis",
        "lung" = "death",
        "lung_ncctg" = "death",
        "time_to_million" = "gross 1M USD",
        "vdv" = "breast cancer",
        "veteran" = "death",
        "mesa_hf" = "heart failure",
        "mesa_chd" = "coronary heart disease",
        "mesa_stroke" = "stroke",
        "mesa_death" = "death",
        "aric_hf" = "heart failure",
        "aric_chd" = "coronary heart disease",
        "aric_stroke" = "stroke",
        "aric_death" = "death",
        "jhs_stroke" = "stroke",
        "jhs_chd" = "coronary heart disease"
      ),
      .before = 2
    )

}

is_double_ish <- function(x){

  is.numeric(x) & length(unique(na.omit(x))) > 12

}


