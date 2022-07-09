#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_pred_clean
#' @param model_key
bench_pred_time_visualize <- function(bm_pred_clean, model_key) {

  gg_data <- bm_pred_clean |>
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
      hjust = if_else(model == 'cif-standard',
                      -1.5,
                      -1/10)
    )



  fig <- ggplot(gg_data) +
    aes(x = time, y = reorder(model, time, FUN=median),
        fill = model) +
    stat_density_ridges(
      quantile_lines = TRUE,
      quantiles = 0.5,
      bandwidth = 0.2
    ) +
    scale_x_log10(
      breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
      labels = c("0.01", "0.1", "1", "10", "100", "1,000", "10,000"),
      expand = c(0,0)
    ) +
    geom_text(
      data = medians,
      hjust = medians$hjust,
      vjust = -.25,
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

  list(medians = medians,
       fig = fig)


}
