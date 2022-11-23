#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_vi_comb
bench_vi_visualize_simple <- function(bm_vi_smry) {

  fig_data <- bm_vi_smry %>%
    group_by(model) %>%
    summarize(
      across(
        .cols = c(
          overall,
          main,
          cmbn,
          nlin,
          intr_main,
          intr_hidden_cmbn,
          intr_hidden_nlin,
          time
        ),
        .fns = mean
      )
    ) %>%
    mutate(
      model = recode(
        model, 'randomForestSRC-permutation' = 'rsf-permutation'
      ),
      model = fct_reorder(model, .x = overall)
    )

  cols <- RColorBrewer::brewer.pal(n = 6, name = 'PuRd')
  text_cols <- rep("black", 6)
  text_cols[2] <- "white"

  fig <- ggplot(fig_data) +
    aes(x = model, y = overall-0.5, fill = model) +
    geom_bar(stat = 'identity', position = 'dodge') +
    theme_bw() +
    scale_y_continuous(breaks = c(0, 0.1, 0.2),
                       labels = c("50%", "60%", "70%")) +
    scale_fill_manual(values = cols) +
    geom_text(aes(label = round(overall,2)),
              vjust = -2/3) +
    geom_text(aes(label = model),
              angle = 90,
              hjust = 'top',
              nudge_x = 1/4,
              nudge_y = -0.01,
              size = 6,
              color = text_cols) +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          legend.position = '',
          text = element_text(size = 12)) +
    labs(x = "", y = "C-statistic")

  ggsave(plot = fig,
         filename = 'poster/fig_vi_smry.pdf',
         device = 'pdf', width = 6, height = 5, dpi = 450)

  NULL

}
