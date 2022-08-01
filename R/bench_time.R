#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

bench_time <- function(data, n_obs=100, n_ftr=10, n_tree = 1) {

  train <- data[seq(n_obs), seq(n_ftr)]

  pred_horizon <- median(train$time)

  node_size <- 10
  mtry <- ceiling(sqrt(n_ftr))


  start_time <- Sys.time()

  aorsf_fast <- orsf(
    data = train,
    n_tree = n_tree,
    formula = Surv(time, status) ~ .,
    mtry = mtry,
    n_retry = 3,
    split_min_obs = node_size,
    control = orsf_control_cph(iter_max = 1,
                               do_scale = FALSE),
    importance = 'none',
    oobag_pred = FALSE
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
    oob.error = FALSE,
    min.node.size = node_size
  )

  end_time <- Sys.time()

  time_fit_ranger <- end_time - start_time


  start_time <- Sys.time()

  cif_party <- cforest(Surv(time, status) ~ .,
                       controls = cforest_unbiased(mtry = mtry),
                       data = train)

  end_time <- Sys.time()

  time_fit_party = end_time - start_time


  list(
    aorsf_fast = time_fit_aorsf,
    rsf_rfsrc = time_fit_rfsrc,
    rsf_ranger = time_fit_ranger,
    cif_party = time_fit_party
  ) %>%
    map(as.numeric, units = 'secs') %>%
    as_tibble() %>%
    mutate(n_obs = n_obs, n_ftr = n_ftr)


}
