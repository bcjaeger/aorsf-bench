
test_that("minimal effect bias", {

  n_obs <- 500000

  ss_demo <-
    sim_surv(
      n_obs = n_obs,
      n_z = 3,
      n_x = 3,
      n_g = 3,
      n_w = 3,
      n_v = 3,
      n_c = 0,
      effect_size_by_group = 1.5
    )

  data_sim <- ss_demo$data

  data_sim$true_v <-
    with(
      data_sim,
      v1 * ss_demo$coefs_lc[[1]][1] +
        v2 * ss_demo$coefs_lc[[1]][2] +
        v3 * ss_demo$coefs_lc[[1]][3]
    )

  mdl <- coxph(
    Surv(time, status) ~
      z1 + z2 + z3 +
      x1*g1 + x2*g2 + x3*g3 +
      pspline(w1) + pspline(w2) + pspline(w3) +
      v2 + v3 + true_v,
    data = data_sim
  )

  effects_expected <- c("x1" = ss_demo$effect_x,
                        "x2" = ss_demo$effect_x,
                        "x3" = ss_demo$effect_x,
                        "g1" = 0,
                        "g2" = 0,
                        "g3" = 0,
                        "x1:g1" = ss_demo$effect_int,
                        "x2:g2" = ss_demo$effect_int,
                        "x3:g3" = ss_demo$effect_int,
                        "v2" = 0,
                        "v3" = 0,
                        "true_v" = ss_demo$effect_lc) |>
    enframe(value = 'truth')

  effects_observed <- coef(mdl) |>
    enframe(value = 'estimate')

  results <- effects_observed |>
    inner_join(effects_expected, by = 'name') |>
    mutate(bias = truth - estimate)

  # termplot(mdl, term=12, se=FALSE, col.term=1, col.se=1)

  expect_lt(max(abs(results$bias)), 0.01)

})


