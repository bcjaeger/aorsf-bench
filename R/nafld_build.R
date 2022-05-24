

nafld_build <- function(){

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

  write_csv(data_all, 'data/nafld.csv')

  NULL

}
