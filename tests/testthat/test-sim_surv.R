
test_that("minimal effect bias", {

  n_obs <- 10000

  ss_demo <-
    sim_surv(
      n_obs = n_obs,
      n_pred_junk = 2,
      n_pred_main = 2,
      n_intr_main = 2,
      n_pred_nlin = 2,
      n_intr_nlin = 2,
      n_pred_cmbn = 2,
      n_intr_cmbn = 1
    )


  data_sim <- ss_demo$data %>%
    mutate(
      hidden_cmbn_1 =
        cmbn_1 * ss_demo$cmbn_key$hidden_cmbn_1$var_coefs[1] +
        cmbn_2 * ss_demo$cmbn_key$hidden_cmbn_1$var_coefs[2]
    )

  mdl <- coxph(
    Surv(time, status) ~
      junk_1 + junk_2 + cmbn_1 +
      main_1 * intr_main_1 +
      main_2 * intr_main_2 +
      pspline(nlin_1) * intr_hidden_nlin_1 +
      pspline(nlin_2) * intr_hidden_nlin_2 +
      hidden_cmbn_1 * intr_hidden_cmbn_1,
    data = data_sim
  )

  effects_expected <- c(
    "junk_1" = 0,
    "junk_2" = 0,
    "cmbn_1" = 0,
    "main_1" = ss_demo$effects$pred_main,
    "main_2" = ss_demo$effects$pred_main,
    "hidden_cmbn_1" = ss_demo$effects$pred_cmbn,
    "main_1:intr_main_1" = ss_demo$effects$intr_main,
    "main_2:intr_main_1" = ss_demo$effects$intr_main,
    "hidden_cmbn_1:intr_hidden_cmbn_1" = ss_demo$effects$intr_cmbn
  ) |>
    enframe(value = 'truth')

  effects_observed <- coef(mdl) |>
    enframe(value = 'estimate')

  results <- effects_observed |>
    inner_join(effects_expected, by = 'name') |>
    mutate(bias = truth - estimate)

  # termplot(mdl, term=c(8, 10), se=FALSE, col.term=1, col.se=1)

  expect_lt(max(abs(results$bias)), 0.1)

})


