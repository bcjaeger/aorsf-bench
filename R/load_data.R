#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source
load_data <- function(data_source,
                      n_obs = NULL,
                      n_z = NULL,
                      correlated_x = NULL) {

  switch(

    data_source,

    'pbc_orsf' = {
      data_all <- aorsf::pbc_orsf |>
        select(-id) |>
        as_tibble()
    },

    'peakV02' = {

      data("peakVO2", package = 'randomForestSRC')

      data_all <- peakVO2 |>
        rename(time = ttodead,
               status = died)

    },

    "veteran" = {

      data("veteran", package = 'randomForestSRC')

      data_all <- veteran

    },

    'rotterdam' = {
      data_all <- survival::rotterdam |>
        mutate(time = pmin(rtime, dtime),
               status = if_else(rtime < dtime, recur, death)) |>
        select(age, meno, size, grade, nodes, pgr, er, hormon,
               time, status) |>
        as_tibble()
    },

    'actg' = {

      data_all <- mlr3proba::actg |>
        as_tibble() |>
        select(-id, -time_d, -censor_d) |>
        rename(status = censor)

    },

    'nafld' = {

      data_1 <- as_tibble(survival::nafld1) |>
        mutate(nafld = as.numeric(case.id == id))

      data_2 <- as_tibble(survival::nafld2) |>
        filter(days <= 0) |>
        group_by(id, days, test) |>
        slice(n()) |>
        pivot_wider(names_from = test, values_from = value)

      data_2_smry <- data_2 |>
        group_by(id) |>
        summarise(days_since_lab = max(days),
                  across(c(chol, hdl, dbp, sbp, fib4, smoke),
                         mean, na.rm = TRUE))

      data_3 <- as_tibble(survival::nafld3) |>
        filter(days <= 0) |>
        arrange(id, event, days) |>
        group_by(id, event) |>
        slice(n()) |>
        pivot_wider(names_from = event, values_from = days) |>
        rename(ang_isc = `ang/isc`,
               heart_failure = `heart failure`,
               cardiac_arrest = `cardiac arrest`) |>
        select(-nafld) |>  # this is derived in data_1
        mutate(across(everything(), ~ if_else(is.na(.x), 0, 1)))

      data_all <- data_1 |>
        left_join(data_2_smry, by = 'id') |>
        left_join(data_3, by = 'id') |>
        mutate(days_since_lab = abs(days_since_lab)) |>
        select(-case.id, -id) |>
        rename(time = futime)


    },

    'colon' = {

      data_all <- survival::colon |>
        select(-id, -study, -node4, -etype) |>
        mutate(sex = factor(sex,
                            levels = c(0,1),
                            labels = c('female', 'male')),
               differ = factor(differ,
                               levels = c(1, 2, 3),
                               labels = c('well', 'moderate', 'poor')))


    },

    'flchain' = {

      data_all <- survival::flchain |>
        rename(time = futime, status = death) |>
        # the chapter variable indicates who died,
        # not fair to use it as a predictor
        select(-chapter) |>
        # 3 died on the same day their sample was collected,
        # let's call that a half day.
        mutate(time = pmax(time, 1/2))

    },

    'guide_it' = {

      data_all <- guide_it_build() |>
        select(-deidnum)

    },

    'breast' = {

      data("Breast",package='biospear')

      data_all <- Breast

      names(data_all)[4:ncol(data_all)] <-
        paste('var',4:ncol(data_all),sep='_')

    },

    'gbsg2' = {

      data("GBSG2", package = "TH.data")

      data_all <- GBSG2 |>
        as_tibble() |>
        rename(status = cens)


    },

    'sim' = {

      data_all <- sim_surv(n_obs = n_obs,
                           n_z = n_z,
                           correlated_x = correlated_x) |>
        getElement('data')

    },

    'sprint-cvd' = {

      data_all <- load_sprint_ndi(outcome = 'cvd')

    },

    'sprint-acm' = {

      data_all <- load_sprint_ndi(outcome = 'acm')

    },

    'time-to-million' = {

      data("time_to_million", package = 'censored')

      data_all <- time_to_million |>
        rename(status = event) |>
        # dropping text and dates
        select(-title, -released, -distributor)

    },

    "vdv" = {

      data("vdv", package = 'randomForestSRC')

      data_all <- vdv |>
        rename(status = Censoring,
               time = Time)

    }

  )

  data_all

}
