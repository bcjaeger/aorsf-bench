## Load your packages, e.g. library(targets).

suppressMessages(source("./packages.R"))

library(future)
library(future.callr)
plan(callr)

tar_config_set(reporter_make = 'summary')

tar_option_set(memory = "transient",
               garbage_collection = TRUE)

# conda config --set ssl_verify no to install pycox packs

## Load your R files
lapply(list.files("./R", full.names = TRUE), base::source)

model_fitters <- c(
  'aorsf_cph_1',
  # 'aorsf_cph_1_filter',
  'aorsf_cph_15',
  'aorsf_random',
  'aorsf_net',
  'rotsf',
  'rsfse',
  'cif',
  'cox_net',
  'coxtime',
  'obliqueRSF',
  'xgb_cox',
  'xgb_aft',
  'randomForestSRC',
  'ranger'
)

analyses_real <- expand_grid(
  data_source = c(
    "veteran",
    "colon_recur",
    "colon_acm",
    "pbc_orsf",
    "time_to_million",
    "gbsg2",
    "peakV02",
    "flchain",
    "nafld",
    "rotterdam_recur",
    "rotterdam_acm",
    "actg_aids",
    "actg_death",
    "guide_it_cvd",
    "guide_it_hfhosp",
    "breast",
    "sprint_cvd",
    "sprint_acm",
    "phts",
    "follic_death",
    "follic_relapse",
    "mgus2_death",
    "mgus2_pcm"
  ),
  model_type = model_fitters,
  run_seed = 1:25
) |>
  mutate(
    data_load_fun = syms(glue("{data_source}_load")),
    model_fit_fun = syms(glue("{model_type}_fit")),
    model_pred_fun = syms(glue("{model_type}_pred"))
  )

analyses_sim <- expand_grid(data_source = 'sim',
                            n_obs = c(1000),
                            n_z = c(20),
                            correlated_x = c(0.3),
                            run_seed = 1:25,
                            model_type = model_fitters) |>
  mutate(
    data_load_fun = syms("sim_surv"),
    model_fit_fun = syms(glue("{model_type}_fit")),
    model_pred_fun = syms(glue("{model_type}_pred"))
  )


tar_plan(

  bm_pred_real <- tar_map(
    values = analyses_real,
    names = c(data_source,
              model_type,
              run_seed),
    tar_target(
      bm_pred_real,
      bench_pred(data_source = data_source,
                 model_type = model_type,
                 data_load_fun = data_load_fun,
                 model_fit_fun = model_fit_fun,
                 model_pred_fun = model_pred_fun,
                 run_seed = run_seed),
      resources = tar_resources(
        future = tar_resources_future(
          resources = list(n_cores=2)
        )
      ),
      memory = "transient",
      garbage_collection = TRUE
    )
  ),

  bm_pred_sim <- tar_map(
    values = analyses_sim,
    names = c(model_type,
              n_obs,
              n_z,
              correlated_x,
              run_seed),
    tar_target(
      bm_pred_sim,
      bench_pred(data_source = data_source,
                 model_type = model_type,
                 data_load_fun = data_load_fun,
                 model_fit_fun = model_fit_fun,
                 model_pred_fun = model_pred_fun,
                 n_obs = n_obs,
                 n_z = n_z,
                 correlated_x = correlated_x,
                 run_seed = run_seed),
      resources = tar_resources(
        future = tar_resources_future(
          resources = list(n_cores=2)
        )
      ),
      memory = "transient",
      garbage_collection = TRUE
    )
  ),

  # bm_vi <- tar_map(
  #   values = analyses_sim,
  #   tar_target(
  #     bm_vi,
  #     bench_vi(data_source = data_source,
  #              n_obs = n_obs,
  #              n_z = n_z,
  #              run_seed = run_seed,
  #              correlated_x = correlated_x),
  #     resources = tar_resources(
  #       future = tar_resources_future(
  #         resources = list(n_cores=2)
  #       )
  #     )
  #   )
  #
  # ),

  tar_target(data_key, summarize_data_source(analyses_real)),

  tar_target(model_key, make_model_key()),

  tar_combine(bm_pred_sim_comb, bm_pred_sim[[1]]),

  tar_combine(bm_pred_real_comb, bm_pred_real[[1]]),

  tar_target(bm_pred_comb, bind_rows(bm_pred_real_comb, bm_pred_sim_comb)),

  tar_target(bm_pred_clean, clean_bm_pred(bm_pred_comb)),

  tar_target(bm_pred_viz, bench_pred_visualize(bm_pred_clean,
                                               data_key,
                                               model_key)),

  tar_target(bm_pred_model, bench_pred_model(bm_pred_clean,
                                             data_key,
                                             model_key)),

  tar_target(bm_pred_model_viz,
             bench_pred_model_visualize(bm_pred_model,
                                        model_key)),

  tar_target(bm_pred_time_viz,
             bench_pred_time_visualize(bm_pred_clean, model_key)),

  # tar_combine(bm_vi_comb, bm_vi[[1]]),
  #
  #
  # tar_target(
  #   bm_vi_smry,
  #   bench_vi_summarize(bm_vi_comb)
  # )

  tar_render(slides, "docs/index.Rmd")


) |>
  tar_hook_before(
    hook = source("conflicts.R"),
    names = everything()
  )




