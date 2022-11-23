#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_time
bm_time_visualize <- function(bm_time) {

  bm_time_smry <- bm_time %>%
    group_by(n_ftr, n_obs) %>%
    # summarize(across(everything(), quantile, probs = c(0.025, .5, .975))) %>%
    # mutate(quant = c('lo', 'est', 'hi')) %>%
    pivot_longer(cols = -c(n_ftr, n_obs)) %>%
    # pivot_wider(names_from = quant, values_from = value) %>%
    mutate(
      name = factor(
        name,
        levels = c('rsf_rfsrc',
                   'aorsf_fast',
                   'rsf_ranger'),
        labels = c('randomForestSRC',
                   'aorsf',
                   'ranger')
      ),
      n_ftr = paste(n_ftr, "predictors")
      # n_times = paste(n_times, "unique event times")
    )

  fig <- ggplot(bm_time_smry, aes(x = n_obs,
                                  y = value,
                                  fill = name,
                                  col = name)) +
    # geom_line() +
    # geom_point() +
    # geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25) +
    geom_smooth(method = 'lm', formula = y~poly(x, 3)) +
    facet_grid(~n_ftr) +
    scale_y_log10() +
    scale_x_log10() +
    labs(y = 'Time to fit 500 trees, seconds',
         x = 'Number of observations in training data',
         color = 'R package',
         fill = 'R package') +
    theme_bw() +
    scale_color_manual(values = c('cyan4', 'purple', 'orange')) +
    scale_fill_manual(values = c('cyan4', 'purple', 'orange')) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())

  ggsave(plot = fig,
         filename = 'temp2.png', width = 10, height = 6, dpi = 300)

  fig


}
