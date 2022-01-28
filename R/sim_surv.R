#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

sim_surv <- function(nobs = 5000, pred_horiz = 2.5) {

  covs <- data.frame(z1 = rnorm(nobs),
                     z2 = rnorm(nobs),
                     z3 = rnorm(nobs),
                     z4 = rnorm(nobs),
                     z5 = rnorm(nobs),
                     z6 = rnorm(nobs),
                     z7 = rnorm(nobs),
                     z8 = rnorm(nobs),
                     z9 = rnorm(nobs),
                     z10 = rnorm(nobs),
                     x1 = rnorm(nobs),
                     x2 = rnorm(nobs),
                     x3 = rnorm(nobs),
                     x4 = rnorm(nobs),
                     x5 = rnorm(nobs),
                     x6 = rnorm(nobs),
                     x7 = rnorm(nobs),
                     x8 = rnorm(nobs),
                     x9 = rnorm(nobs),
                     x10 = rnorm(nobs),
                     g1 = rnorm(nobs), #rbinom(nobs, 1, 1/2),
                     g2 = rnorm(nobs), #rbinom(nobs, 1, 1/2),
                     g3 = rnorm(nobs), #rbinom(nobs, 1, 1/2),
                     g4 = rnorm(nobs), #rbinom(nobs, 1, 1/2),
                     g5 = rnorm(nobs)) #rbinom(nobs, 1, 1/2))

  covs$int_1 = with(covs, x1 * g1)
  covs$int_2 = with(covs, x2 * g2)
  covs$int_3 = with(covs, x3 * g3)
  covs$int_4 = with(covs, x4 * g4)
  covs$int_5 = with(covs, x5 * g5)

  s1 <- simsurv(lambdas = 0.1,
                gammas = 1.5,
                betas = c(z1  = -1/3,
                          z2  = -1/3,
                          z3  = -1/3,
                          z4  = -1/3,
                          z5  = -1/3,
                          z6  = -1/3,
                          z7  = -1/3,
                          z8  = -1/3,
                          z9  = -1/3,
                          z10 = -1/3,
                          x1  = 1/2,
                          x2  = 1/2,
                          x3  = 1/2,
                          x4  = 1/2,
                          x5  = 1/2,
                          x6  = 1/2,
                          x7  = 1/2,
                          x8  = 1/2,
                          x9  = 1/2,
                          x10 = 1/2,
                          g1 = 0,
                          g2 = 0,
                          g3 = 0,
                          g4 = 0,
                          g5 = 0,
                          int_1 = 1/2,
                          int_2 = 1/2,
                          int_3 = 1/2,
                          int_4 = 1/2,
                          int_5 = 1/2),
                x = covs,
                maxt = pred_horiz * 2)

  keep <- grep(pattern = '^z|^x|^g', x = names(covs))

  data_out <- as.data.frame(cbind(s1, covs[, keep]))
  data_out$id <- NULL
  data_out

}
