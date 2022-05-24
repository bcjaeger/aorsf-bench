## Load your packages, e.g. library(targets).
source("./packages.R")

library(future)
library(future.callr)
plan(callr)


# conda config --set ssl_verify no to install pycox packs

## Load your R files
lapply(list.files("./R", full.names = TRUE), base::source)

model_fitters <- c(
  'aorsf_1',
  'aorsf_15',
  'aorsf_net',
  'cif',
  # 'coxtime',
  # 'obliqueRSF',
  # 'xgboost',
  'randomForestSRC',
  'ranger'
)

analyses_real <- expand_grid(
  data_source = c(
    "vdv",
    "veteran",
    "colon",
    "pbc_orsf",
    'time_to_million',
    'gbsg2',
    # 'peakV02',
    # 'flchain',
    # 'nafld',
    "rotterdam",
    "actg",
    "guide_it",
    "breast"
    # "sprint_cvd",
    # "sprint_acm"
  ),
  model_type = model_fitters,
  run_seed = 1:3
) |>
  mutate(
    data_load_fun = case_when(
      data_source == "vdv"             ~ syms("load_vdv"),
      data_source == "veteran"         ~ syms("load_veteran"),
      data_source == "colon"           ~ syms("load_colon"),
      data_source == "pbc_orsf"        ~ syms("load_pbc_orsf"),
      data_source == "time_to_million" ~ syms("load_time_to_million"),
      data_source == "gbsg2"           ~ syms("load_gbsg2"),
      data_source == "peakV02"         ~ syms("load_peakV02"),
      data_source == "flchain"         ~ syms("load_flchain"),
      data_source == "nafld"           ~ syms("load_nafld"),
      data_source == "rotterdam"       ~ syms("load_rotterdam"),
      data_source == "actg"            ~ syms("load_actg"),
      data_source == "guide_it"        ~ syms("load_guide_it"),
      data_source == "breast"          ~ syms("load_breast"),
      data_source == "sprint_cvd"      ~ syms("load_sprint_cvd"),
      data_source == "sprint_acm"      ~ syms("load_sprint_acm")
    ),
    model_fit_fun = case_when(
      model_type == 'aorsf_1'         ~ syms("aorsf_cph_1_fit"),
      model_type == 'aorsf_15'        ~ syms("aorsf_cph_15_fit"),
      model_type == 'randomForestSRC' ~ syms("randomForestSRC_fit"),
      model_type == 'ranger'          ~ syms("ranger_fit"),
      model_type == 'aorsf_net'       ~ syms("aorsf_net_fit"),
      model_type == 'cif'             ~ syms("cif_fit"),
      model_type == 'coxtime'         ~ syms("coxtime_fit"),
      model_type == 'obliqueRSF'      ~ syms("obliqueRSF_fit"),
      model_type == 'xgboost'         ~ syms("xgboost_fit"),
      model_type == 'randomForestSRC' ~ syms("randomForestSRC_fit"),
    ),
    model_prd_fun = case_when(
      model_type == 'aorsf_1'         ~ syms("aorsf_cph_1_pred"),
      model_type == 'aorsf_15'        ~ syms("aorsf_cph_15_pred"),
      model_type == 'randomForestSRC' ~ syms("randomForestSRC_pred"),
      model_type == 'ranger'          ~ syms("ranger_pred"),
      model_type == 'aorsf_net'       ~ syms("aorsf_net_pred"),
      model_type == 'cif'             ~ syms("cif_pred"),
      model_type == 'coxtime'         ~ syms("coxtime_pred"),
      model_type == 'obliqueRSF'      ~ syms("obliqueRSF_pred"),
      model_type == 'xgboost'         ~ syms("xgboost_pred"),
      model_type == 'randomForestSRC' ~ syms("randomForestSRC_pred"),
    ),
    n_obs = NA_real_,
    n_z = NA_real_,
    correlated_x = NA_real_
  )

analyses_sim <- expand_grid(data_source = 'sim',
                            n_obs = c(1000),
                            n_z = c(20),
                            correlated_x = c(0.1, 0.3),
                            run_seed = 1:50)


tar_plan(

  bm_pred <- tar_map(
    values = analyses_real, #bind_rows(analyses_real, analyses_sim),
    names = c(data_source,
              model_type,
              n_obs,
              n_z,
              correlated_x,
              run_seed),
    tar_target(
      res_pred,
      bench_pred(data_source = data_source,
                 model_type = model_type,
                 data_load_fun = data_load_fun,
                 model_fit_fun = model_fit_fun,
                 model_prd_fun = model_prd_fun,
                 n_obs = n_obs,
                 n_z = n_z,
                 correlated_x = correlated_x,
                 run_seed = run_seed),
      resources = tar_resources(
        future = tar_resources_future(
          resources = list(n_cores=3)
        )
      )
    )
  ),

  # bm_vi <- tar_map(
  #   values = analyses_sim,
  #   tar_target(res_vi, bench_vi(data_source = data_source,
  #                               n_obs = n_obs,
  #                               n_z = n_z,
  #                               run_seed = run_seed,
  #                               correlated_x = correlated_x))
  #
  # ),

  tar_target(
    data_key,
    summarize_data_source(x = unique(analyses_real$data_source))
  ),


  tar_combine(bm_pred_comb, bm_pred[[1]])

  # tar_combine(bm_vi_comb, bm_vi[[1]])

  #
  # tar_target(
  #   benchmark_vi_smry,
  #   bench_vi_summarize(benchmark_vi_comb)
  # )

)




