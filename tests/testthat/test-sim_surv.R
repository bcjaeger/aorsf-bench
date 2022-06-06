
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("minimal effect bias", {

  bias_df <- df <- data.frame(matrix(ncol = 12, nrow = 0))

  n_obs_per <- 2000

  for(i in 1:100) {

    ss_demo <-
      sim_surv(
        n_obs = n_obs_per,
        n_z = 0,
        n_x = 3,
        n_g = 0,
        n_w = 3,
        n_v = 0,
        # omit categorical variables; these are under construction
        n_c = 0
      )

    data_sim <- ss_demo$data

    # data_sim$true_v <-
    #   with(
    #     data_sim,
    #     v1 * ss_demo$coefs_lc[[1]][1] +
    #       v2 * ss_demo$coefs_lc[[1]][2] +
    #       v3 * ss_demo$coefs_lc[[1]][3]
    #   )

    data_means <- data_sim |>
      select(where(is.numeric)) |>
      apply(2, mean) |>
      round(3) |>
      enframe()

    # test that close to 45% of data is censored
    # expect_equal(data_means$status, 0.55, tolerance = 0.03)

    ##
    dd <- datadist(data_sim)
    options(datadist = dd)

    mdl <- cph(
      Surv(time, status) ~
        # z1 + z2 + z3 +
        # x1*g1 + x2*g2 + x3*g3 +
        x1 + x2 + x3 +
        rcs(w1) + rcs(w2) + rcs(w3),
        # v2 + v3 + true_v,
      data = data_sim
    )

    ##
    effects_expected <- c("x1" = 2,
                          "x2" = 2,
                          "x3" = 2,
                          "g1" = 0,
                          "g2" = 0,
                          "g3" = 0,
                          "x1 * g1" = 2,
                          "x2 * g2" = 2,
                          "x3 * g3" = 2,
                          "v2" = 0,
                          "v3" = 0,
                          "true_v" = 2)

    effects_observed <- coef(mdl)[names(effects_expected)]

      # the differences between observed and expected estimates is small
    biases <- (effects_expected - effects_observed) |>
      enframe(name = 'predictor',
              value = 'bias')

    bias_df[nrow(bias_df)+1, ] <- biases$bias
    colnames(bias_df) <- biases$predictor
  }

  average_biases <- apply(bias_df, MARGIN=2, FUN=mean)
  bias_in_tolerance <- unlist(lapply(average_biases, function(x) {x < 0.01}))

  expect_equal(all(bias_in_tolerance), TRUE)

})


