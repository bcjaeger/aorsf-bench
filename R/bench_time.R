

#' Benchmark of prediction time based on simulated data
#'
#' @param n_obs number of observations in the data
#' @param n_ftr number of predictors in the data
#' @param n_tree number of trees on the ensemble
#' @param n_times number of times to measure computing time
#'
#' @details
#' Added this experiment to the paper after a productive
#'  conversation with reviewers on rOpenSci.
#'  https://github.com/ropensci/software-review/issues/532/#issuecomment-1238748885
#'
bench_time <- function(n_obs = 100,
                       n_ftr = 10,
                       n_tree = 500,
                       n_times = NULL) {

  # I made this data with sim_surv() and saved it in data/ to make
  # this experiment run faster. I.e., instead of calling sim_surv()
  # every time I call bench_time(), I just make one really big data
  # set with sim_surv(), save it, and then read in subsets of it
  # for time benchmarking with different data dimensions.

  data <- read_rds("data/bench_time.rds")[seq(n_obs), ]

  if(n_ftr == 10000){
    for(i in seq(9000)){
      data[[paste("junk", i, sep="_")]] <- rnorm(n = nrow(data))
    }
  }

  train <- data[, seq(n_ftr + 2)]

  node_size <- round(n_obs / 10)

  mtry <- ceiling(sqrt(n_ftr))

  if(!is.null(n_times) && length(unique(train$time)) > n_times){
    train$time <- 5 * as.numeric(cut(train$time, breaks = n_times)) / n_times
  }

  start_time <- Sys.time()

  aorsf_fast <- orsf(
    data = train,
    n_tree = n_tree,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 1,
    n_split = 5,
    split_min_obs = node_size,
    control = orsf_control_fast(),
    importance = 'none',
    oobag_pred_type = 'none'
  )

  end_time <- Sys.time()

  time_fit_aorsf <- end_time - start_time

  start_time <- Sys.time()

  rsf_rfsrc <- rfsrc(Surv(time, status) ~ .,
                     ntree = n_tree,
                     samptype = 'swr',
                     perf.type = 'none',
                     data = train,
                     mtry = mtry,
                     nsplit = 5,
                     ntime = 0,
                     nodesize = node_size)

  end_time <- Sys.time()

  time_fit_rfsrc <- end_time - start_time

  start_time <- Sys.time()

  rsf_ranger <- ranger(
    Surv(time, status) ~ .,
    num.trees = n_tree,
    splitrule = 'extratrees',
    data = train,
    mtry = mtry,
    num.random.splits = 5,
    oob.error = FALSE,
    min.node.size = node_size
  )

  end_time <- Sys.time()

  time_fit_ranger <- end_time - start_time

  list(
    aorsf_fast = time_fit_aorsf,
    rsf_rfsrc = time_fit_rfsrc,
    rsf_ranger = time_fit_ranger
    # cif_party = time_fit_party
  ) %>%
    map(as.numeric, units = 'secs') %>%
    as_tibble() %>%
    mutate(n_obs = n_obs, n_ftr = n_ftr, n_times = n_times)


}
