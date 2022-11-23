

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_source
#' @param run_seed
bench_vi_real <- function(data_source,
                          data_load_fun,
                          run_seed,
                          test_prop = 1/2) {

  set.seed(run_seed)

  data_all <- data_load_fun()

  # Some R packages (not aorsf) have trouble
  # with factors that have special characters
  data_all <-  data_all |>
    mutate(across(where(is.character), as.factor),
           across(where(is.factor), simplify_levels))

  test_index <- sample(x = seq(nrow(data_all)),
                       size = round(nrow(data_all) * test_prop),
                       replace = FALSE)

  train <- data_all[-test_index, ]
  test <- data_all[test_index, ]

  imputer <- recipe(x = train, time + status ~ .) %>%
    step_nzv(all_predictors()) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep()

  .train <- as.data.frame(juice(imputer))
  .test <- as.data.frame(bake(imputer, new_data = test))

  event_time_bounds <- quantile(
    x = data_all$time[data_all$status==1],
    probs = c(.25, .75)
  )

  pred_horizon <- sort(unique(.test$time[.test$status == 1]))
  pred_horizon <- pred_horizon[pred_horizon <= event_time_bounds['75%']]
  pred_horizon <- pred_horizon[pred_horizon >= event_time_bounds['25%']]

  if(is_empty(pred_horizon)) return(NULL)

  if(length(pred_horizon) > 30){
    pred_horizon <-
      pred_horizon[floor(seq(1, length(pred_horizon), length.out=30))]
  }

  fit_orsf <- orsf(.train, time + status ~ ., control = orsf_control_cph())
  fit_xgb <- xgb_cox_fit(.train, pred_horizon = pred_horizon)
  fit_rsf <- rfsrc(Surv(time, status) ~ .,
                   samptype = 'swr',
                   importance = 'permute',
                   data = .train)

  # half of the predictors
  n_vars <- round((ncol(.train) - 2) / 2)


  .test_mat <- as.matrix(select(.test, -time, -status))

  vars_shap <-
    predict(fit_xgb$fit, newdata = .test_mat, predcontrib = TRUE) |>
    apply(MARGIN = 2, function(x) mean(abs(x))) %>%
    sort(decreasing = TRUE)

  vars_shap <- vars_shap[-which(names(vars_shap)=='BIAS')]
  vars_shap <- names(vars_shap)[seq(n_vars)]

  vars_permute_rsf <-
    names(sort(fit_rsf$importance, decreasing = TRUE))[seq(n_vars)]

  vars_anova   <- names(orsf_vi_anova(fit_orsf))[seq(n_vars)]
  vars_permute_orsf <- names(orsf_vi_permute(fit_orsf))[seq(n_vars)]
  vars_negate  <- names(orsf_vi_negate(fit_orsf))[seq(n_vars)]

  fits_orsf_final <-
    list(
      orsf_anova_orsf = vars_anova,
      orsf_permute_orsf = vars_permute_orsf,
      orsf_negate_orsf = vars_negate,
      orsf_permute_rsf = vars_permute_rsf,
      orsf_shap_xgb = vars_shap
    ) %>%
    map(na.omit) %>%
    map(
      .f = ~ orsf(.train[, c('time', 'status', .x)],
                  formula = time + status ~ .)
    )

  predictions_orsf <- map(
    fits_orsf_final,
    .f = ~ predict(.x, new_data = .test, pred_horizon = pred_horizon)
  )

  fits_rsf_final <-
    list(
      rsf_anova_orsf = vars_anova,
      rsf_permute_orsf = vars_permute_orsf,
      rsf_negate_orsf = vars_negate,
      rsf_permute_rsf = vars_permute_rsf,
      rsf_shap_xgb = vars_shap
    ) %>%
    map(na.omit) %>%
    map(
      .f = ~ rfsrc(data = .train[, c('time', 'status', .x)],
                   formula = Surv(time + status) ~ .)
    )

  predictions_rsf <- map(
    fits_rsf_final,
    .f = ~ predictRisk(.x,
                       newdata = .test,
                       times = pred_horizon)
  )

  fits_xgb_final <-
    list(
      xgb_anova_orsf   = vars_anova,
      xgb_permute_orsf = vars_permute_orsf,
      xgb_negate_orsf  = vars_negate,
      xgb_permute_rsf  = vars_permute_rsf,
      xgb_shap_xgb     = vars_shap
    ) %>%
    map(na.omit) %>%
    map(
      .f = ~ xgb_cox_fit(.train[, c('time', 'status', .x)],
                         pred_horizon = pred_horizon)
    )

  predictions_xgb <- map2(
    fits_xgb_final,
    list(vars_anova,
         vars_permute_orsf,
         vars_negate,
         vars_permute_rsf,
         vars_shap),
    .f = ~ xgb_cox_pred(.x, .test[, c('time', 'status', .y)], pred_horizon)
  ) %>%
    map("pred")

  sc <- try(
    Score(
      object = c(predictions_orsf, predictions_rsf, predictions_xgb),
      formula = Surv(time, status) ~ 1,
      data = select(.test, time, status),
      summary = c('IPA', 'ibs'),
      times = pred_horizon,
      se.fit = FALSE,
      contrasts = FALSE
    ),
    silent = TRUE
  )

  score <- tibble(model = names(predictions),
                  cstat = NA_real_,
                  ibs_scaled = NA_real_)

  if( !inherits(sc, 'try-error') ) {

    cstat <- sc$AUC$score |>
      group_by(model) |>
      summarize(cstat = mean(AUC))

    brier <- sc$Brier$score |>
      group_by(model) |>
      slice(n()) |>
      ungroup() |>
      mutate(
        ibs_scaled = (IBS[model=='Null model'] - IBS) /
          IBS[model=='Null model']
      ) %>%
      filter(model != 'Null model') %>%
      select(model, ibs_scaled)

    score <- left_join(cstat, brier)

  }

  out <- score %>%
    mutate(data = data_source,
           run = run_seed,
           .before = cstat)

  # print(arrange(out, desc(ibs_scaled)))

  out

}



