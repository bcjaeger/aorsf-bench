#' @description Visualize the benchmark of prediction models.
#'
#' @details Inference on equivalence and superiority.
#'
#' @param bm_pred_model a target created from the benchmark experiment
#' @param model_key a target with labels about model information
#' @param equiv_bound a number specifying the boundary for equivalence.
#'   E.g., equiv_bound of 0.01 means anything within 0.01 of a given
#'   reference value is considered practically equivalent.
#'
bench_pred_model_visualize <- function(bm_pred_model,
                                       model_key,
                                       equiv_bound = 0.01) {

  data_infer <- bm_pred_model$posterior

  gg_data <-
    data_infer |>
    filter(model != 'coxtime') |>
    group_by(metric, model) |>
    summarize(
      prob_equiv = mean(value <= equiv_bound & value >= -equiv_bound),
      prob_super = mean(value < 0),
      prob_super_duper = mean(value < -equiv_bound),
      median = median(value),
      q25 = quantile(value, probs = 1/4),
      q75 = quantile(value, probs = 3/4),
      ci_lwr = quantile(value, probs = .025),
      ci_upr = quantile(value, probs = .975)
    ) |>
    arrange(desc(median)) |>
    mutate(
      x = rev(seq(n())),
      model = recode(model, !!!deframe(model_key)),
      across(
        .cols = median:ci_upr,
        .fns = ~if_else(is.infinite(.x), 0, .x)
      ),
      across(
        starts_with("prob"),
        ~ format(round(.x, 2), nsmall=2)
      ),
      across(
        starts_with("prob"),
        ~ str_replace(.x, '1.00', '>.999')
      ),
      across(
        starts_with("prob"),
        ~ if_else(model == 'aorsf-fast', '---', .x)
      ),
      metric = factor(metric,
                      levels = c("ibs_scaled", "cstat"),
                      labels = c("Index of prediction accuracy",
                                 "Time-dependent C-statistic"))
    )

  write_rds(gg_data, "../seminar-fastpack/eval_fig_data.rds")

  plts <- gg_data |>
    group_by(metric) |>
    group_split() |>
    set_names(c("ibs_scaled", "cstat")) %>%
    map(bench_pred_model_visualize_worker,
        equiv_bound = equiv_bound)

  list(fig = plts,
       data = gg_data %>%
         mutate(
           metric = recode(
             metric,
             "Index of prediction accuracy" = 'ibs_scaled',
             "Time-dependent C-statistic" = 'cstat'
           )
         )
       )


}

# simplifying the function above by separating most of the work
# into its own function
bench_pred_model_visualize_worker <- function(.gg_data,
                                              equiv_bound){

  xmax <- max(.gg_data$x)

  y_col_0 = -equiv_bound * 10
  y_col_1 = equiv_bound * 4
  y_col_2 = equiv_bound * 7
  y_col_3 = equiv_bound * 10

  y_breaks <- seq(-5, 1) * 1/100

  gg_header <- tibble(
    x = c(xmax + 1,
          xmax + 1,
          xmax + 2,
          xmax + 1,
          xmax + 1,
          xmax + 1),
    median = c(y_col_0,
               (min(y_breaks) + max(y_breaks)) / 2,
               (y_col_1 + y_col_3) / 2,
               y_col_1,
               y_col_2,
               y_col_3),
    label = c("Learner",
              as.character(.gg_data$metric[1]),
              "Posterior probability",
              "Equivalence",
              "Difference < 0",
              "Difference < -1"),
    hjust = c(0, 1/2, 1/2, 1/2, 1/2, 1/2)
  )

  gg_rect <- tibble(
    xmin = seq(xmax+1) - 1/2,
    xmax = seq(xmax+1) + 1/2,
    ymin = -Inf,
    ymax = Inf
  ) |>
    filter(seq(n()) %% 2 == 0)

  ggplot(.gg_data) +
    aes(x = x, y = median, label = model) +
    geom_segment(x = 0, y = -.01, xend = xmax+1/2, yend = -.01,
                 color = 'purple', linetype = 2) +
    geom_segment(x = 0, y = .01, xend = xmax+1/2, yend = .01,
                 color = 'purple', linetype = 2) +
    geom_segment(x = 0, y = 0, xend = xmax+1/2, yend = 0,
                 color = 'darkorange', linetype = 2) +
    geom_vline(xintercept = c(xmax + 1/2, xmax + 3/2)) +
    geom_segment(
      mapping = aes(x = x,
                    y = ci_lwr,
                    xend = x,
                    yend = ci_upr),
      size = 0.75,
      color = 'grey80'
    ) +
    geom_segment(
      mapping = aes(x = x,
                    y = q25,
                    xend = x,
                    yend = q75),
      size = 2,
      color = 'grey60'
    ) +
    geom_rect(data = gg_rect,
              inherit.aes = FALSE,
              aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'grey',
              alpha = 1/6) +
    geom_segment(x = 0, xend = 0,
                 y = min(y_breaks), yend = max(y_breaks)) +
    geom_point(size = 3, color = 'darkorange') +
    geom_text(aes(y = y_col_0), hjust = 0) +
    geom_text(aes(x = x, y = y_col_1, label = prob_equiv)) +
    geom_text(aes(x = x, y = y_col_2, label = prob_super)) +
    geom_text(aes(x = x, y = y_col_3, label = prob_super_duper)) +
    geom_text(data = gg_header, aes(label = label, hjust = hjust)) +
    scale_y_continuous(limits = c(y_col_0, y_col_0*(-1)*1.25),
                       breaks = y_breaks,
                       labels = 100 * y_breaks,
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, xmax + 2.5),
                       expand = c(0, 0)) +
    coord_flip() +
    theme_bw() +
    labs(x = '', y = 'Difference versus aorsf-fast') +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(hjust = .34),
          legend.position = '')




}

# a smaller version of the function above for simpler figures
bench_pred_model_visualize_smol <- function(.gg_data,
                                            equiv_bound){

  xmax <- max(.gg_data$x)

  y_col_0 = -equiv_bound * 10
  y_col_1 = equiv_bound * 4

  y_breaks <- seq(-5, 1) * 1/100

  gg_header <- tibble(
    x = c(xmax + 1,
          xmax + 1),
    median = c(y_col_0,
               (min(y_breaks) + max(y_breaks)) / 2),
    label = c("Learner",
              as.character(.gg_data$metric[1])),
    hjust = c(0, 1/2)
  )

  gg_rect <- tibble(
    xmin = seq(xmax+1) - 1/2,
    xmax = seq(xmax+1) + 1/2,
    ymin = -Inf,
    ymax = Inf
  ) |>
    filter(seq(n()) %% 2 == 0)

  ggplot(.gg_data) +
    aes(x = x, y = median, label = model) +
    geom_segment(x = 0, y = -.01, xend = xmax+1/2, yend = -.01,
                 color = 'purple', linetype = 2) +
    geom_segment(x = 0, y = .01, xend = xmax+1/2, yend = .01,
                 color = 'purple', linetype = 2) +
    geom_segment(x = 0, y = 0, xend = xmax+1/2, yend = 0,
                 color = 'darkorange', linetype = 2) +
    geom_vline(xintercept = c(xmax + 1/2, xmax + 3/2)) +
    geom_segment(
      mapping = aes(x = x,
                    y = ci_lwr,
                    xend = x,
                    yend = ci_upr),
      size = 0.75,
      color = 'grey80'
    ) +
    geom_segment(
      mapping = aes(x = x,
                    y = q25,
                    xend = x,
                    yend = q75),
      size = 2,
      color = 'grey60'
    ) +
    geom_rect(data = gg_rect,
              inherit.aes = FALSE,
              aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'grey',
              alpha = 1/6) +
    geom_segment(x = 0, xend = 0,
                 y = min(y_breaks), yend = max(y_breaks)) +
    geom_point(size = 3, color = 'darkorange') +
    geom_text(aes(y = y_col_0), hjust = 0) +
    geom_text(data = gg_header, aes(label = label, hjust = hjust)) +
    scale_y_continuous(limits = c(y_col_0, 0.03),
                       breaks = y_breaks,
                       labels = 100 * y_breaks,
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, xmax + 2.5),
                       expand = c(0, 0)) +
    coord_flip() +
    theme_bw() +
    labs(x = '', y = 'Difference versus aorsf-fast') +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(hjust = .34),
          legend.position = '')


  # ggsave(
  #   '../bcjaeger-website/content/project/penguins/featured-hex.png',
  #   dpi = 450,
  #   width = 4.5,
  #   height = 4.5
  # )

}

