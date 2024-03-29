## Load your packages, e.g. library(targets).

suppressMessages(source("./packages.R"))

library(future)
library(future.callr)
plan(callr)

tar_config_set(reporter_make = 'verbose')

tar_option_set(memory = "transient",
               garbage_collection = TRUE)

# conda config --set ssl_verify no to install pycox packs

## Load your R files
lapply(list.files("./R", full.names = TRUE), base::source)

model_fitters <- c(
  'aorsf_fast',
  'aorsf_cph',
  'aorsf_random',
  # 'aorsf_net',
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
    "nki",
    "lung",
    "lung_ncctg",
    "follic_death",
    "follic_relapse",
    "mgus2_death",
    "mgus2_pcm",
    "mesa_hf",
    "mesa_chd",
    "mesa_stroke",
    "mesa_death",
    "aric_hf",
    "aric_chd",
    "aric_stroke",
    "aric_death",
    "jhs_stroke",
    "jhs_chd"
  ),
  model_type = model_fitters,
  run_seed = 1:25
) |>
  mutate(
    data_load_fun = syms(glue("{data_source}_load")),
    model_fit_fun = syms(glue("{model_type}_fit")),
    model_pred_fun = syms(glue("{model_type}_pred"))
  )

analyses_real_public <- analyses_real %>%
  filter(data_source %in% c("veteran",
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
                            "breast",
                            "nki",
                            "lung",
                            "lung_ncctg",
                            "follic_death",
                            "follic_relapse",
                            "mgus2_death",
                            "mgus2_pcm"))

# Uncomment the line below if you would like to use only public data
# analyses_real <- analyses_real_public

analyses_sim_pred <- expand_grid(data_source = 'sim',
                                 n_obs = c(500, 1000, 2500),
                                 pred_corr_max = c(0, 0.15, 0.30),
                                 run_seed = 1:500,
                                 model_type = model_fitters) %>%
  mutate(
    data_load_fun = syms("sim_surv"),
    model_fit_fun = syms(glue("{model_type}_fit")),
    model_pred_fun = syms(glue("{model_type}_pred"))
  )

analyses_sim_vi <- analyses_sim_pred %>%
  select(-starts_with('model'), -starts_with('data')) %>%
  distinct()

analyses_real_vi <- analyses_real %>%
  select(data_source, data_load_fun, run_seed) %>%
  distinct()

tar_plan(

  bm_pred_real <- tar_map(
    values = analyses_real,
    names = c(data_source,
              model_type,
              run_seed),
    tar_target(
      bm_pred_real,
      bench_pred_real(data_source = data_source,
                      model_type = model_type,
                      data_load_fun = data_load_fun,
                      model_fit_fun = model_fit_fun,
                      model_pred_fun = model_pred_fun,
                      run_seed = run_seed),
      resources = tar_resources(
        future = tar_resources_future(
          resources = list(n_cores=4)
        )
      ),
      memory = "transient",
      garbage_collection = TRUE
    )
  ),

  bm_vi_real <- tar_map(
    values = analyses_real_vi,
    names = c(data_source, run_seed),
    tar_target(
      bm_vi_real,
      bench_vi_real(data_source = data_source,
                    data_load_fun = data_load_fun,
                    run_seed = run_seed),
      resources = tar_resources(
        future = tar_resources_future(
          resources = list(n_cores=1)
        )
      ),
      memory = "transient",
      garbage_collection = TRUE
    )
  ),
  tar_combine(bm_vi_real_comb, bm_vi_real),

  tar_target(time_runs, seq(5)),
  tar_target(n_obs, round(10^seq(from = 2, to = 4, by = 1/3))),
  tar_target(n_ftr, c(10, 100, 1000)),
  # tar_target(n_times, c(1000, 10000)),

  # bm_time_data = sim_surv(n_obs = max(n_obs),
  #                         n_pred_main = 1000) %>%
  #   getElement("data") %>%
  #   select(time,
  #          status,
  #          starts_with("main")),

  tar_target(
    bm_time,
    bench_time(n_obs = n_obs, n_ftr = n_ftr),# n_times = n_times),
    pattern = cross(time_runs, n_obs, n_ftr),# n_times),
    resources = tar_resources(
      future = tar_resources_future(
        resources = list(n_cores=4)
      )
    )
  ),

  tar_target(
    bm_time_viz,
    bm_time_visualize(bm_time)
  ),

  # bm_pred_sim <- tar_map(
  #   values = analyses_sim_pred,
  #   names = c(model_type,
  #             n_obs,
  #             pred_corr_max,
  #             run_seed),
  #   tar_target(
  #     bm_pred_sim,
  #     bench_pred_sim(data_source = data_source,
  #                    model_type = model_type,
  #                    data_load_fun = data_load_fun,
  #                    model_fit_fun = model_fit_fun,
  #                    model_pred_fun = model_pred_fun,
  #                    n_obs = n_obs,
  #                    pred_corr_max = pred_corr_max,
  #                    run_seed = run_seed),
  #     resources = tar_resources(
  #       future = tar_resources_future(
  #         resources = list(n_cores=4)
  #       )
  #     ),
  #     memory = "transient",
  #     garbage_collection = TRUE
  #   )
  # ),

  bm_vi <- tar_map(
    values = analyses_sim_vi,
    tar_target(
      bm_vi,
      bench_vi(n_obs = n_obs,
               pred_corr_max = pred_corr_max,
               run_seed = run_seed),
      resources = tar_resources(
        future = tar_resources_future(
          resources = list(n_cores=4)
        )
      )
    )

  ),

  clincalc_r2 = replicate(
    n = 100,
    expr = sim_surv(n_obs = max(analyses_sim_vi$n_obs)) %>%
      getElement('variation_explained'),
    simplify = TRUE
  ),

  tar_target(data_key, summarize_data_source(analyses_real)),

  tar_target(model_key, make_model_key()),

  tar_combine(bm_pred_real_comb, bm_pred_real[[1]]),

  tar_target(bm_pred_clean, clean_bm_pred(bm_pred_real_comb)),

  tar_target(bm_pred_viz, bench_pred_visualize(bm_pred_clean$data,
                                               data_key,
                                               model_key)),

  tar_target(bm_pred_model, bench_pred_model(bm_pred_clean$data,
                                             data_key,
                                             model_key)),

  tar_target(bm_pred_model_viz,
             bench_pred_model_visualize(bm_pred_model,
                                        model_key)),

  tar_target(bm_pred_time_viz,
             bench_pred_time_visualize(bm_pred_clean, model_key)),

  tar_combine(bm_vi_comb, bm_vi[[1]]),

  tar_target(bm_vi_viz, bench_vi_visualize(bm_vi_comb)),

  tar_target(bm_vi_smry, bench_vi_summarize(bm_vi_comb)),

  # tar_target(bm_vi_viz_simple, bench_vi_visualize_simple(bm_vi_comb)),

  tar_target(bm_vi_viz_mean, bench_vi_mean_visualize(bm_vi_comb))


) |>
  tar_hook_before(
    hook = source("conflicts.R"),
    names = everything()
  )




