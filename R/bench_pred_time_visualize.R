#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_pred_clean
#' @param model_key
bench_pred_time_visualize <- function(bm_pred_clean, model_key) {

  gg_data <- bm_pred_clean$data |>
    select(model, time_fit, time_pred, data) |>
    drop_na() |>
    mutate(time = as.numeric(time_fit) + as.numeric(time_pred),
           model = recode(model, !!!deframe(model_key)),
           model = fct_reorder(model, .x = time, .fun = median))

  medians <- gg_data |>
    group_by(model) |>
    summarize(time = median(time)) %>%
    mutate(
      color = c(
        rep("white", floor(n()/2)),
        rep("black", n() - floor(n()/2))
      ),

      color = case_when(
        model == 'ranger-extratrees' ~ 'black',
        # model == 'aorsf-cph' ~ 'black',
        TRUE ~ color
      ),

      hjust = -1/10,

      vjust = case_when(
        model == 'ranger-extratrees' ~ -1.25,
        # model == 'aorsf-cph' ~ -1.25,
        model == 'cif-standard' ~ -1.35,
        TRUE ~ -1/4
      )
    )

  write_rds(gg_data, '../seminar-fastpack/time_fig_data.rds')
  write_rds(medians, '../seminar-fastpack/time_fig_medians.rds')


  model_subsets <- list(
    slide_one = c('ranger-extratrees',
                  'rsf-standard',
                  'cif-standard',
                  'obliqueRSF-net'),
    slide_two = c('aorsf-fast',
                  'ranger-extratrees',
                  'rsf-standard',
                  'cif-standard',
                  'obliqueRSF-net'),
    paper = levels(gg_data$model)
  )

  figs <- model_subsets %>%
    map(
      ~ fig_worker(
        data = filter(gg_data, model %in% .x),
        medians = filter(medians, model %in% .x)
      )
    )

  # ggsave(figs$slide_one,
  #        filename = '../seminar-fastpack/img/aorsf-bench-time_1.png',
  #        dpi = 300,
  #        width = 5.5,
  #        height = 4.5)
  #
  # ggsave(figs$slide_two,
  #        filename = '../seminar-fastpack/img/aorsf-bench-time_2.png',
  #        dpi = 300,
  #        width = 5.5,
  #        height = 4.5)

  list(medians = medians,
       fig = figs$paper)


}

fig_worker <- function(data, medians){

  fig <- ggplot(data) +
    aes(x = time, y = reorder(model, time, FUN=median),
        fill = model) +
    stat_density_ridges(
      quantile_lines = TRUE,
      quantiles = 0.5,
      bandwidth = 0.2,
      scale = 1
    ) +
    scale_x_log10(
      breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
      labels = c("0.01", "0.1", "1", "10", "100", "1,000", "10,000"),
      expand = c(0,0)
    ) +
    geom_text(
      data = medians,
      hjust = medians$hjust,
      vjust = medians$vjust,
      color = medians$color,
      aes(label = table_glue("{time}s"))
    ) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = '') +
    labs(x = 'Time to fit a model and compute predictions, seconds',
         y = '')


}

