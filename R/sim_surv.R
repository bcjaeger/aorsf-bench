#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

sim_surv <- function(n_obs = 2500,
                     n_z = 20,
                     n_x = 20,
                     n_g = 20,
                     n_w = 20,
                     n_v = 20,
                     n_c = 10,
                     correlated_x = 0,
                     pred_horiz = 2.5) {

  stopifnot(n_x > 0)
  stopifnot(n_z > 0)
  stopifnot(n_v >= 3)

  stopifnot(n_x >= n_g)

  z_names <- paste0('z', seq(n_z))
  x_names <- paste0('x', seq(n_x))
  g_names <- paste0('g', seq(n_g))
  w_names <- paste0('w', seq(n_w))
  v_names <- paste0('v', seq(n_v))
  c_names <- paste0('c', seq(n_c))

  .names <- c(z_names, x_names)

  if(n_g > 0) .names <- c(.names, g_names)

  if(n_w > 0) .names <- c(.names, w_names)

  if(n_v > 0) .names <- c(.names, v_names)

  if(n_c > 0) .names <- c(.names, c_names)

  n_vars <- n_z + n_x + n_g + n_w + n_v + n_c

  if(correlated_x > 0){

    mat_covar <- matrix(0, nrow = n_vars, ncol = n_vars)

    diag(mat_covar) <- 1

    colnames(mat_covar) <- rownames(mat_covar) <- .names

    # Make the Z variables correlate with the others

    mat_covar[x_names, z_names] <-
      runif(n = n_z * n_x,
            min = -correlated_x,
            max = correlated_x)

    # symmetry
    mat_covar[upper.tri(mat_covar)] <- t(mat_covar)[upper.tri(mat_covar)]

    mat_covar <- Matrix::nearPD(mat_covar)$mat

    covs <- mvtnorm::rmvnorm(n = n_obs, sigma = as.matrix(mat_covar))


  } else {

    covs <- matrix(
      rnorm(n = n_vars * n_obs),
      ncol = n_vars,
      nrow = n_obs
    )

  }

  colnames(covs) <- .names

  covs <- as.data.frame(covs)

  # Interaction variables ----

  vars_interaction <- c()

  if(n_g > 0){

    for( i in seq(n_g) ){

      int_name <- paste0("int_", i)
      x_name <- paste0('x', i)
      g_name <- paste0('g', i)

      vars_interaction <- c(vars_interaction, paste(x_name,
                                                    g_name,
                                                    sep = '-'))

      covs[[int_name]] <- covs[[x_name]] * covs[[g_name]]

    }

  }

  # Non-linear variables ----

  if(n_w > 0){

    amplitudes <- seq(1/4, 1/2, length.out = n_w)

    for( i in seq(n_w) ){

      nl_name <- paste0("nl_", i)
      w_name <- paste0("w", i)

      covs[[nl_name]] <- sin(amplitudes[i] * pi * covs[[w_name]])

    }

  }

  # linear combination variables ----

  random_sign <- function(x) {

    rb <- rbinom(n = length(x), size = 1, prob = 1/2)

    out <- x

    if(any(rb == 0)) out[rb == 0] <- out[rb == 0] * (-1)

    out

  }

  n_lc <- 0

  vars_lc <- c()
  coefs_lc <- list()
  coefs_counter <- 1

  if(n_v > 0){

    lc_assign <- rep(seq(n_v / 3), times = n_v)
    lc_assign <- lc_assign[seq(n_v)]

    n_lc <- length(unique(lc_assign))

    for( i in unique(lc_assign) ){

      lc_vars <- v_names[lc_assign == i]

      vars_lc <- c(vars_lc, paste(lc_vars, collapse = '-'))

      lc_coefs <- length(lc_vars) |>
        runif(min = 1/2, max = 1) |>
        random_sign()

      coefs_lc[[coefs_counter]] <- lc_coefs
      coefs_counter <- coefs_counter+1

      lc_value <- as.matrix(covs[, lc_vars]) %*% as.matrix(lc_coefs)

      lc_name <- paste0('lc_', paste(lc_vars, collapse = '_'))

      covs[lc_name] <- lc_value

    }

  }

  betas <- vector(mode = 'numeric', length = ncol(covs))
  names(betas) <- names(covs)

  if(n_x > 0)  x_effect   <- 3 / n_x else x_effect   <- 0
  if(n_g > 0)  int_effect <- 3 / n_g else int_effect <- 0
  if(n_w > 0)  nl_effect  <- 3 / n_w else nl_effect  <- 0
  if(n_v > 0)  lc_effect  <- 3 / n_v else lc_effect  <- 0
  if(n_c > 0)  c_effect   <- 3 / n_c else c_effect   <- 0

  betas[str_detect(names(betas), '^x')] <- x_effect

  if(n_g > 0)
    betas[str_detect(names(betas), '^int')] <- int_effect

  if(n_w > 0)
    betas[str_detect(names(betas), '^nl')] <- nl_effect

  if(n_lc > 0)
    betas[str_detect(names(betas), '^lc')] <- lc_effect

  if(n_c > 0)
    betas[str_detect(names(betas), '^c')] <- c_effect

  s1 <- simsurv(lambdas = 0.1,
                gammas = 1.5,
                betas = betas,
                x = covs,
                maxt = pred_horiz * 2)

  # after generating outcomes using continuous values of c variables,
  # cut those variables into categories

  if(n_c > 0){
    for(.c in c_names){

      covs[[.c]] <- cut(covs[[.c]],
                        breaks = c(-Inf, -1/2, 1/2, Inf),
                        labels = letters[1:3])

    }
  }




  keep <- grep(pattern = '^z|^x|^g|^w|^v|^c', x = names(covs))

  vars_signal <- names(betas)[grep(pattern = '^x|^g|^w|^v|^c',
                                   x = names(betas))]

  list(
    data = as_tibble(cbind(s1, covs[, keep])) |>
      rename(time = eventtime) |>
      select(-id),
    vars_signal = vars_signal,
    vars_junk = setdiff(names(covs)[keep], vars_signal),
    vars_interaction = vars_interaction,
    vars_lc = vars_lc,
    coefs_lc = coefs_lc
  )


}



# testing

# sim <- sim_surv(n_obs = 5000,
#                 n_z = 1,
#                 n_x = 1,
#                 n_g = 1,
#                 n_w = 1)
#
# data <- sim$data
#
# dd <- datadist(data)
#
# options(datadist = dd)
#
# m <- cph(Surv(time, status) ~ z1 + x1 * g1 + rcs(w1), data = data)
#
# anova(m)
#
# ggplot(Predict(m, w1))
