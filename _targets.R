## Load your packages, e.g. library(targets).
source("./packages.R")

library(future)
library(future.callr)
plan(callr)


# conda config --set ssl_verify no to install pycox packs

## Load your R files
lapply(list.files("./R", full.names = TRUE), base::source)

analyses_real <- expand_grid(data_source = c("pbc_orsf",
                                             "rotterdam"),
                             run_seed = 1:50)

analyses_sim <- expand_grid(data_source = 'sim',
                            n_obs = c(1000),
                            n_z = c(20),
                            correlated_x = c(0.1),
                            run_seed = 1:50)


tar_plan(

  bm_pred <- tar_map(
    values = bind_rows(analyses_sim, analyses_real),
    tar_target(res_pred, bench_pred(data_source = data_source,
                                    n_obs = n_obs,
                                    n_z = n_z,
                                    correlated_x = correlated_x,
                                    run_seed = run_seed))
  ),

  bm_vi <- tar_map(
    values = analyses_sim,
    tar_target(res_vi, bench_vi(data_source = data_source,
                                n_obs = n_obs,
                                n_z = n_z,
                                run_seed = run_seed,
                                correlated_x = correlated_x))

  ),


  tar_combine(bm_pred_comb, bm_pred[[1]]),

  tar_combine(bm_vi_comb, bm_vi[[1]])

  #
  # tar_target(
  #   benchmark_vi_smry,
  #   bench_vi_summarize(benchmark_vi_comb)
  # )

)




