#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source


load_vdv <- function(...){

  data("vdv", package = 'randomForestSRC')

  vdv |>
    rename(status = Censoring,
           time = Time)


}

load_veteran <- function(...){

  data("veteran", package = 'randomForestSRC')
  veteran

}
load_colon <- function(...){

  survival::colon |>
    select(-id, -study, -node4, -etype) |>
    mutate(sex = factor(sex,
                        levels = c(0,1),
                        labels = c('female', 'male')),
           differ = factor(differ,
                           levels = c(1, 2, 3),
                           labels = c('well', 'moderate', 'poor')))

}
load_pbc_orsf <- function(...){

  aorsf::pbc_orsf |>
    select(-id) |>
    as_tibble()

}
load_time_to_million <- function(...){

  data("time_to_million", package = 'censored')

  time_to_million |>
    rename(status = event) |>
    # dropping text and dates
    select(-title, -released, -distributor)

}
load_gbsg2 <- function(...){

  data("GBSG2", package = "TH.data")

  GBSG2 |>
    as_tibble() |>
    rename(status = cens)

}
load_peakV02 <- function(...){

  data("peakVO2", package = 'randomForestSRC')

  peakVO2 |>
    rename(time = ttodead, status = died)


}
load_flchain <- function(...){

  survival::flchain |>
    rename(time = futime, status = death) |>
    # the chapter variable indicates who died,
    # not fair to use it as a predictor
    select(-chapter) |>
    # 3 died on the same day their sample was collected,
    # let's call that a half day.
    mutate(time = pmax(time, 1/2))

}

load_nafld <- function(...){

  read_csv('data/nafld.csv')

}

load_rotterdam <- function(...){

  survival::rotterdam |>
    mutate(time = pmin(rtime, dtime),
           status = if_else(rtime < dtime, recur, death)) |>
    select(age, meno, size, grade, nodes, pgr, er, hormon,
           time, status) |>
    as_tibble()

}
load_actg <- function(...){

  mlr3proba::actg |>
    as_tibble() |>
    select(-id, -time_d, -censor_d) |>
    rename(status = censor)

}
load_guide_it <- function(...){

  guide_it_build() |>
    select(-deidnum)

}
load_breast <- function(...){

  data("Breast",package='biospear')

  data_all <- Breast

  names(data_all)[4:ncol(data_all)] <-
    paste('var',4:ncol(data_all),sep='_')

  data_all

}
load_sprint_cvd <- function(...){
  load_sprint_ndi(outcome = 'cvd')
}
load_sprint_acm <- function(...){
  load_sprint_ndi(outcome = 'acm')
}

load_sim <- function(n_obs,
                     n_z,
                     correlated_x){

  sim_surv(n_obs = n_obs,
           n_z = n_z,
           correlated_x = correlated_x) |>
    getElement('data')

}
