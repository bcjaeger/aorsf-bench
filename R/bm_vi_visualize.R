#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bm_vi_comb
bm_vi_visualize <- function(bm_vi_comb) {

  bm_vi_smry <- bm_vi_comb %>%
    filter(pred_corr_max != .45) %>%
    arrange(n_obs, pred_corr_max) %>%
    mutate(
      intr = (intr_main+intr_hidden_cmbn+intr_hidden_nlin)/3,
    ) %>%
    pivot_longer(cols = c(cmbn,
                          main,
                          nlin,
                          intr),
                 names_to = 'variable') %>%
    group_by(variable, n_obs, pred_corr_max, model) %>%
    summarize(value = mean(value, na.rm = TRUE))

  bm_vi_overall <- bm_vi_comb %>%
    group_by(model) %>%
    summarize(value = mean(overall)) %>%
    mutate(variable = 'Overall')

  slide_in <- bm_vi_smry$variable %>%
    unique() %>%
    enframe(value = 'variable', name = NULL) %>%
    split(.$variable)

  data_smry_split <-
    bind_rows(bm_vi_overall, bm_vi_smry) %>%
    mutate(variable = factor(variable, levels = c('Overall',
                                                  'main',
                                                  'cmbn',
                                                  'nlin',
                                                  'intr'))) %>%
    arrange(variable, n_obs, pred_corr_max, desc(value)) %>%
    split(.$variable)

  slide_in

  for(i in names(data_smry_split)){

    if(i %in% names(slide_in)){

      data_smry_split[[i]] <- bind_rows(
        slide_in[[i]],
        data_smry_split[[i]]
      )

    }

  }

  .order <-
    c(
      "Overall",
      data_smry_split %>%
        bind_rows(.id = 'variable') %>%
        group_by(variable) %>%
        summarize(m = median(value, na.rm = TRUE)) %>%
        arrange(m) %>%
        filter(variable != 'Overall') %>%
        pull(variable)
    )

  data_smry_pre <- data_smry_split[.order] %>%
    bind_rows() %>%
    mutate(x = paste(variable, n_obs, pred_corr_max, sep = '_'),
           x = fct_inorder(x),
           x = fct_rev(x),
           x = as.numeric(x)) %>%
    group_by(variable, n_obs, pred_corr_max) %>%
    mutate(
      x = x + (n() > 1) * seq(-1/4, 1/4, length.out = n()),
      data = paste(variable, n_obs, pred_corr_max, sep = '_')
    )

  y_col_1 = 0.15
  y_col_2 = 0.30

  xmax <- max(ceiling(data_smry$x))

  # gg_text_header <- tibble(
  #   x = c(xmax + 3/4, xmax + 3/4),
  #   eval = c(y_col_1, (min(y_breaks) + max(y_breaks)) / 2),
  #   label = c("Dataset; outcome", eval_label),
  #   hjust = c(0, 1/2)
  # )

  gg_text_data <- data_smry_pre %>%
    mutate(x = round(x)) %>%
    group_by(variable, n_obs, pred_corr_max, data, x) %>%
    summarize(x = median(x)) %>%
    mutate(
      value_1 = y_col_1,
      value_2 = y_col_2,
      variable = recode(
        variable,
        "nlin"    = "Non-linear effects",
        "main"    = "Main effects",
        "cmbn"    = "Combination effects",
        "intr"    = "Interactions"
      ),
      label_1 = if_else(
        is.na(n_obs),
        variable,
        as.character(
          if_else(
            pred_corr_max > 0,
            " ",
            table_value(n_obs)
          )
        )
      ),
      label_2 = if_else(
        is.na(n_obs),
        "",
        as.character(pred_corr_max * 100)
      )
    ) %>%
    ungroup() %>%
    add_row(x = xmax + 1,
            value_2 = y_col_2,
            label_2 = 'Max correlation', .before = 1) %>%
    add_row(x = xmax + 1,
            value_2 = y_col_1,
            label_2 = 'No. observations', .before = 1)

  data_aorsf <- data_smry |>
    filter(model == 'aorsf-negate')

  rankings <- data_smry |>
    arrange(variable, n_obs, pred_corr_max, desc(value)) |>
    mutate(rank = seq(n())) %>%
    mutate(data = paste(variable, n_obs, pred_corr_max, sep = '_'))

  aorsf_wins <- rankings |>
    filter(rank == 1 & model == 'aorsf-negate') |>
    pull(data)

  gg_text_diffs <- rankings |>
    filter(data %in% aorsf_wins, rank <= 2) %>%
    group_by(data) %>%
    group_split() |>
    map_dfr(
      .f = ~{

        tibble(data = .x$data[1],
               pdiff = perc_diff(.x$value[1], .x$value[2]),
               adiff = diff(rev(.x$value)))

      }
    ) |>
    left_join(gg_text_data) |>
    mutate(
      value = map_dbl(
        data,
        ~data_aorsf |>
          filter(data == .x) |>
          pull(value)
      ),
      label = table_glue("+{100*adiff} ({pdiff}%)")
    )

  data_smry <- drop_na(data_smry_pre, value) %>%
    filter(model != 'aorsf-negate')

  rect_x_values <- gg_text_data %>%
    filter(is.na(n_obs), variable != 'Overall') %>%
    pull(x)

  gg_rect <- tibble(
    xmin = rect_x_values - 1/2,
    xmax = rect_x_values + 1/2,
    ymin = -Inf,
    ymax = Inf
  )

  model_levels <- bm_vi_overall %>%
    arrange(value) %>%
    filter(model != 'aorsf-negate') %>%
    pull(model)

  data_smry$model <- factor(data_smry$model, levels = model_levels)

  oranges <- brewer.pal(n = length(model_levels), "Oranges")
  standout <- "purple"

  gg_fig <- ggplot(data_smry) +
    aes(x = x, y = value) +
    geom_point(shape = 21,
               size = 2,
               aes(fill = model)) +
    geom_point(data = data_aorsf,
               shape = 21,
               size = 3,
               fill = standout) +
    geom_text(data = gg_text_data,
              aes(y = value_1, label = label_1),
              hjust = 0) +
    geom_text(data = gg_text_data,
              aes(y = value_2, label = label_2),
              hjust = 0) +
    geom_text(data = gg_text_diffs,
              aes(label = label),
              hjust = -0.2,
              color = standout) +
    geom_rect(data = gg_rect,
              inherit.aes = FALSE,
              aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'black',
              alpha = 1/6) +
    geom_vline(xintercept = seq(1/2, xmax-1/2, by = 1)) +
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = 'top') +
    scale_fill_manual(values = oranges) +
    labs(x = '', y = '')

  gg_fig



  +
    scale_fill_manual(values = oranges) +
    scale_y_continuous(limits = c(y_col_1, max(data_fig$eval)*1.15),
                       expand = c(0, 0),
                       breaks = y_breaks,
                       labels = function(x) x * 100) +
    scale_x_continuous(limits = c(0, xmax + 2),
                       expand = c(0, 0)) +
    geom_segment(x = 0, xend = 0,
                 y = 0, yend = max(y_breaks)) +
    geom_rect(data = gg_rect,
              inherit.aes = FALSE,
              aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              fill = 'grey',
              alpha = 1/6) +
    labs(x = '', y = '')


}
