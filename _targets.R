## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

analyses <- expand_grid(source = c("pbc_orsf", "rotterdam","sim"),
                        run_seed = 1:25)



## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  res <- tar_map(
    values = analyses,
    tar_target(results, analysis_run(source = source,
                                     run_seed = run_seed))
  ),

  tar_combine(
    data_results,
    res[[1]],
    command = bind_rows(!!!.x)
  ),

)
