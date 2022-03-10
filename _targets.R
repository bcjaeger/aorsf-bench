## Load your packages, e.g. library(targets).
source("./packages.R")

library(future)
library(future.callr)
plan(callr)


# conda config --set ssl_verify no to install pycox packs

## Load your R files
lapply(list.files("./R", full.names = TRUE), base::source)

analyses_pred <- expand_grid(data_source = c("pbc_orsf",
                                             "rotterdam",
                                             "sim"),
                             run_seed = 1:100)

analyses_vi <- expand_grid(data_source = 'sim',
                           n_obs = c(2500, 5000),
                           n_z = c(20),
                           correlated_x = c(0, 0.1, 0.2, 0.3, 0.4),
                           run_seed = 1:50)


## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  # benchmark_pred <- tar_map(
  #   values = analyses_pred,
  #   tar_target(res_pred, bench_pred(data_source = data_source,
  #                                   run_seed = run_seed))
  # ),

  benchmark_vi <- tar_map(
    values = analyses_vi,
    tar_target(res_vi, bench_vi(data_source = data_source,
                                n_obs = n_obs,
                                n_z = n_z,
                                run_seed = run_seed,
                                correlated_x = correlated_x))

  ),

  tar_combine(
    benchmark_vi_comb,
    benchmark_vi[[1]],
    command = bind_rows(!!!.x)
  ),

  tar_target(
    benchmark_vi_smry,
    bench_vi_summarize(benchmark_vi_comb)
  )

)




