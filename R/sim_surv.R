#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

sim_surv <- function(n_obs = 500,
                     n_pred_junk = 15,
                     n_pred_main = 15,
                     n_intr_main = 5,
                     n_pred_nlin = 15,
                     n_intr_nlin = 5,
                     n_pred_cmbn = 45,
                     n_intr_cmbn = 5,
                     eff_size_pred_main = 3/4,
                     eff_size_intr_main = 3/4,
                     eff_size_pred_nlin = 3/4,
                     eff_size_intr_nlin = 3/4,
                     eff_size_pred_cmbn = 3/4,
                     eff_size_intr_cmbn = 3/4,
                     pred_corr_min = 0.00,
                     pred_corr_max = 0.10,
                     pred_horiz = 2.5) {

  stopifnot(n_pred_main >= n_intr_main)
  stopifnot(n_pred_nlin >= n_intr_nlin)
  stopifnot(n_pred_cmbn / 2 >= n_intr_cmbn)
  stopifnot(pred_corr_min <= pred_corr_max)

  # no effect
  junk_names <- paste('junk', seq(n_pred_junk), sep = '_')

  # main effect
  main_names <- paste('main', seq(n_pred_main), sep = '_')

  # no effect apart from interaction with a main effect
  intr_main_names <- paste('intr_main', seq(n_intr_main), sep = '_')

  # non-linear effect
  nlin_names <- paste('nlin', seq(n_pred_nlin), sep = '_')

  # no effect apart from interaction with a non-linear effect
  intr_nlin_names <- paste('intr_hidden_nlin', seq(n_intr_nlin), sep = '_')

  # combination linear effect
  cmbn_names <- paste('cmbn', seq(n_pred_cmbn), sep = '_')

  # no effect apart from interaction with a combination effect
  intr_cmbn_names <- paste('intr_hidden_cmbn', seq(n_intr_cmbn), sep = '_')

  .names <- c()

  if (n_pred_junk > 0) .names <- c(.names, junk_names)
  if (n_pred_main > 0) .names <- c(.names, main_names)
  if (n_pred_nlin > 0) .names <- c(.names, nlin_names)
  if (n_pred_cmbn > 0) .names <- c(.names, cmbn_names)
  if (n_intr_main > 0) .names <- c(.names, intr_main_names)
  if (n_intr_nlin > 0) .names <- c(.names, intr_nlin_names)
  if (n_intr_cmbn > 0) .names <- c(.names, intr_cmbn_names)

  n_vars <-
    n_pred_junk +
    n_pred_main +
    n_pred_nlin +
    n_pred_cmbn +
    n_intr_main +
    n_intr_nlin +
    n_intr_cmbn

  stopifnot(n_vars > 0)

  mat_covar <- diag(x = n_vars)

  colnames(mat_covar) <- rownames(mat_covar) <- .names

  if(pred_corr_max > 0){

    for( i in seq(n_vars) ){

      for (j in seq(i, n_vars)){

        if(j > i){
          mat_covar[i, j] <-
            runif(n = 1,
                  min = pred_corr_min,
                  max = pred_corr_max) *
            sample(x = c(-1, 1), size = 1)

          mat_covar[j, i] <- mat_covar[i, j]

        }

      }

    }

  # nearest positive definite matrix (must be invertible for rmvnorm)
  mat_covar <- Matrix::nearPD(mat_covar)$mat

  }

  covs <- mvtnorm::rmvnorm(n = n_obs, sigma = as.matrix(mat_covar))

  colnames(covs) <- .names

  data_covs <- as.data.frame(covs) %>%
    add_attribute(attr_name = 'key', attr_value = list()) %>%
    add_nonlinear_effects(n_pred_nlin) %>%
    add_combination_effects(cmbn_names) %>%
    add_interactions(n_intr = n_intr_main, type = 'main') %>%
    add_interactions(n_intr = n_intr_nlin, type = 'hidden_nlin') %>%
    add_interactions(n_intr = n_intr_cmbn, type = 'hidden_cmbn')

  # making effect sizes -----

  betas <- vector(mode = 'numeric', length = ncol(data_covs))

  names(betas) <- names(data_covs)


  main_indx <- names(betas) %>%
    str_detect('^main_[0-9]+$')

  intr_main_indx <- names(betas) %>%
    str_detect('^hidden_main_[0-9]+_x_intr_main_[0-9]+$')

  nlin_indx <- names(betas) %>%
    str_detect('^hidden_nlin_[0-9]+$')

  intr_nlin_indx <- names(betas) %>%
    str_detect('^hidden_hidden_nlin_[0-9]+_x_intr_hidden_nlin_[0-9]+$')

  cmbn_indx <- names(betas) %>%
    str_detect('^hidden_cmbn_[0-9]+$')

  intr_cmbn_indx <- names(betas) %>%
    str_detect('^hidden_hidden_cmbn_[0-9]+_x_intr_hidden_cmbn_[0-9]+$')

  betas[main_indx]      <- eff_size_pred_main
  betas[intr_main_indx] <- eff_size_intr_main
  betas[nlin_indx]      <- eff_size_pred_nlin
  betas[intr_nlin_indx] <- eff_size_intr_nlin
  betas[cmbn_indx]      <- eff_size_pred_cmbn
  betas[intr_cmbn_indx] <- eff_size_intr_cmbn

  s1 <- simsurv(lambdas = 0.1,
                gammas = 1.5,
                betas = betas,
                x = data_covs,
                maxt = pred_horiz * 2)

  drop <- grep(pattern = '^hidden', x = names(data_covs))

  data <- as_tibble(cbind(s1, data_covs[, -drop])) |>
    rename(time = eventtime) |>
    select(-id)

  mdl_formula <- paste(
    paste(main_names, "*", intr_main_names, collapse = ' + '),
    paste0("splines::bs(", nlin_names,  ") * ", intr_nlin_names, collapse = ' + '),
    paste(cmbn_names, "*", intr_cmbn_names, collapse = ' + '),
    sep = ' + '
  ) %>%
    paste("Surv(time, status) ~ ", .) %>%
    as.formula()

  variation_explained <- mdl_formula %>%
    coxph(data = data, x = TRUE) %>%
    coxph_err() %>%
    getElement('ERR')

  list(
    data = data,
    cmbn_key = attr(data_covs, 'key'),
    effects = list(pred_main = eff_size_pred_main,
                   intr_main = eff_size_intr_main,
                   pred_nlin = eff_size_pred_nlin,
                   intr_nlin = eff_size_intr_nlin,
                   pred_cmbn = eff_size_pred_cmbn,
                   intr_cmbn = eff_size_intr_cmbn),
    variation_explained = variation_explained
  )


}

add_interactions <- function(covs, n_intr, type){

  if(n_intr == 0) return(covs)

  for( i in seq(n_intr) ){

    covs <- covs %>%
      add_interaction(name_v1 = glue('{type}_{i}'),
                      name_v2 = glue('intr_{type}_{i}'))

  }

  covs

}

add_interaction <- function(covs, name_v1, name_v2){

  iname <- paste('hidden', name_v1, 'x', name_v2, sep = '_')

  covs[[iname]] <- covs[[name_v1]] * covs[[name_v2]]

  covs

}

add_nonlinear_effects <- function(covs, n_pred_nlin){

  if(n_pred_nlin == 0) return(covs)

  amplitudes <- seq(1/8, 1/4, length.out = n_pred_nlin)

  for( i in seq(n_pred_nlin) ){

    covs <- covs %>%
      add_nonlinear_effect(amplitude = amplitudes[i],
                           name_v1 = glue("nlin_{i}"))

  }

  covs

}

add_nonlinear_effect <- function(covs, amplitude, name_v1){

    nl_name <- paste('hidden', name_v1, sep = '_')

    covs[[nl_name]] <- sin(amplitude * pi * covs[[name_v1]])

    covs


}

add_combination_effects <- function(covs, cmbn_names, min_size = 3){

  if(length(cmbn_names) == 0) return(covs)

  n_grps <- round(length(cmbn_names) / min_size)

  cmbn_grps <- split(cmbn_names,
                     f = factor(seq_along(cmbn_names) %%  n_grps))

  for(i in seq_along(cmbn_grps)){


    .names <- cmbn_grps[[i]]
    .coefs <- runif(n = length(.names), min = 1/2, max = 3/4) *
      random_sign(size = length(.names))

    new_key <- list(
      x = list(var_names = .names,
               var_coefs = .coefs)
    )

    names(new_key) <- glue("hidden_cmbn_{i}")

    attr(covs, 'key') = c( attr(covs, 'key'), new_key)

    covs <- covs %>%
      add_combination_effect(.names = .names,
                             .coefs = .coefs,
                             .hidden_name = glue("hidden_cmbn_{i}"))

  }

  covs

}

add_combination_effect <- function(covs, .names, .coefs, .hidden_name){

  covs[[.hidden_name]] <- as.numeric(
    as.matrix(covs[, .names]) %*% as.matrix(.coefs, ncol = 1)
  )

  covs

}

random_sign <- function(size) {

  sample(x = c(-1, 1), size = size, replace = TRUE)

}
