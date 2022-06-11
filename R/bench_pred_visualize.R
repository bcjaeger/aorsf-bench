perc_diff <- function(a,b){
  100 * (a-b) / b
}

bench_pred_visualize <- function(bm_pred_clean,
                                 data_key,
                                 model_key){


  data_recoder <- data_key |>
    transmute(data, label = paste(label, outcome, sep = '; ')) |>
    deframe()

  model_recoder <- deframe(model_key)

  list(
    ibs_scaled = bench_pred_visualize_(
      filter(bm_pred_clean, data != 'sim'),
      data_recoder,
      model_recoder,
      f_smry = mean,
      eval_metric = 'ibs_scaled',
      y_col_1 = -0.4,
      y_min = 0,
      y_breaks = seq(0, .6, by = .1),
      eval_label = 'Index of Prediction Accuracy'
    ),
    cstat = bench_pred_visualize_(
      filter(bm_pred_clean, data != 'sim'),
      data_recoder,
      model_recoder,
      f_smry = mean,
      eval_metric = 'cstat',
      y_col_1 = 0.35,
      y_min = .55,
      y_breaks = seq(.7, 1, by = .1),
      eval_label = 'C-statistic'
    )
  )


}

bench_pred_visualize_ <- function(
    bm_pred_clean,
    data_recoder,
    model_recoder,
    f_smry = mean,
    eval_metric = 'ibs_scaled',
    y_col_1 = -0.4,
    y_min = 0,
    y_breaks = seq(0, .6, by = .1),
    eval_label = 'Index of Prediction Accuracy'
){

  bm_pred_inner <- bm_pred_clean |>
    rename(eval = .data[[eval_metric]])

  if(eval_metric == 'ibs_scaled'){
    bm_pred_inner <- bm_pred_inner |>
      filter(model != 'xgb_aft') |>
      droplevels()
  }

  data_fig_overall <- bm_pred_inner |>
    mutate(data = fct_reorder(factor(data), .x = eval)) |>
    group_by(model) |>
    summarize(eval = f_smry(eval)) |>
    mutate(data = 'Overall')

  data_levels <- c(
    "Overall",
    levels(
      with(
        bm_pred_inner,
        fct_reorder( factor(data),  .x = eval )
      )
    )
  )

  data_fig <- bm_pred_inner |>
    group_by(model, data) |>
    summarize(eval = f_smry(eval)) |>
    bind_rows(data_fig_overall) |>
    mutate(data = factor(data, levels = data_levels)) |>
    arrange(data, desc(eval)) |>
    mutate(x = as.numeric(fct_rev(data)),
           data = recode(data, !!!data_recoder)) |>
    select(model, data, x, eval) |>
    group_by(data) |>
    mutate(x = x + seq(-1/3, 1/3, length.out = n()))

  gg_text_data <- data_fig |>
    group_by(data) |>
    summarize(x = median(x)) |>
    mutate(eval = y_col_1)

  data_aorsf <- data_fig |>
    filter(model == 'aorsf_cph_1')

  aorsf_wins <- data_fig |>
    filter(model != 'aorsf_cph_15',
           model != 'aorsf_cph_1_filter',
           model != 'aorsf_net',
           model != 'obliqueRSF') |>
    arrange(data, desc(eval)) |>
    mutate(rank = seq(n())) |>
    filter(rank == 1 & model == 'aorsf_cph_1') |>
    pull(data)

  gg_text_diffs <- data_fig |>
    filter(model != 'aorsf_cph_15',
           model != 'aorsf_cph_1_filter',
           model != 'aorsf_net',
           model != 'obliqueRSF') |>
    arrange(data, desc(eval)) |>
    mutate(rank = seq(n())) |>
    filter(data %in% aorsf_wins, rank <= 2) |>
    group_split() |>
    map_dfr(
      .f = ~{

        tibble(data = .x$data[1],
               pdiff = perc_diff(.x$eval[1], .x$eval[2]),
               adiff = diff(rev(.x$eval)))

      }
    ) |>
    left_join(gg_text_data) |>
    mutate(
      eval = map_dbl(
        data,
        ~data_aorsf |>
          filter(data == .x) |>
          pull(eval)
      ),
      label = table_glue("+{100*adiff} ({pdiff}%)")
    )

  data_fig <- data_fig |>
    filter(!str_detect(model, '^aorsf|obliqueRSF$')) |>
    droplevels()

  model_levels <- data_fig |>
    filter(data == 'Overall') |>
    arrange(eval) |>
    pull(model) |>
    as.character() |>
    c('aorsf_cph_1')

  data_fig <- data_fig |>
    filter(eval > y_min)

  blues <- brewer.pal(n = length(model_levels) - 1, "Oranges")
  standout <- "purple"


  gg_legend <- ggpubr::get_legend(
    data_fig |>
      bind_rows(data_aorsf) |>
      mutate(model = factor(model,
                            levels = model_levels,
                            labels = model_recoder[model_levels])) |>
      ggplot(aes(x=x, y=eval, col=model)) +
      geom_point() +
      scale_color_manual(values = c(blues, standout)) +
      theme_bw() +
      theme(legend.position = 'top') +
      labs(color = '') +
      guides(colour = guide_legend(nrow = 1,
                                   override.aes = list(size=8)))
  )

  data_fig$model <- factor(
    data_fig$model,
    levels = setdiff(model_levels, 'aorsf_cph_1')
  )

  xmax <- max(data_fig$x)

  gg_text_header <- tibble(
    x = c(xmax + 3/4, xmax + 3/4),
    eval = c(y_col_1, (min(y_breaks) + max(y_breaks)) / 2),
    label = c("Dataset; outcome", eval_label),
    hjust = c(0, 1/2)
  )

  gg_rect <- tibble(
    xmin = seq(xmax+1) - 1/2,
    xmax = seq(xmax+1) + 1/2,
    ymin = -Inf,
    ymax = Inf
  ) |>
    filter(seq(n()) %% 2 == 0)

  gg_fig <- ggplot(data_fig) +
    aes(x = x, y = eval) +
    geom_point(shape = 21,
               size = 2,
               aes(fill = model)) +
    geom_point(data = data_aorsf,
               shape = 21,
               size = 3,
               fill = standout) +
    geom_text(data = gg_text_data,
              aes(label = data),
              hjust = 0) +
    geom_text(data = gg_text_diffs,
              aes(label = label),
              hjust = -0.2,
              color = standout) +
    geom_text(data = gg_text_header,
              aes(label = label,
                  hjust = hjust)) +
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = '') +
    scale_fill_manual(values = blues) +
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

  cowplot::plot_grid(
    gg_legend,
    gg_fig,
    nrow = 2,
    rel_heights = c(0.075, 0.935)
  )

}


#
#
# to_omit <- bm_pred_clean |>
#   filter(is.na(cstat) | is.na(IPA)) |>
#   distinct(data, run) |>
#   as.list() |>
#   map(unique)
#
# bm_pred_clean <- bm_pred_clean |>
#   filter( ! (data %in% to_omit$data & run %in% to_omit$run) )
#
# data_recoder <- data_key |>
#   select(data, label) |>
#   deframe()
#
# data_fig_overall <- bm_pred_clean |>
#   droplevels() |>
#   mutate(data = fct_reorder(factor(data), .x = IPA)) |>
#   group_by(model) |>
#   summarize(
#     across(
#       .cols = c(cstat, IPA, time_fit, time_pred),
#       .fns = list(mean = mean, sd = sd)
#     )
#   ) |>
#   mutate(data = 'Overall')
#
# data_levels <- c(
#   "Overall",
#   levels(
#     with(
#       bm_pred_clean,
#       fct_reorder( factor(data),  .x = IPA )
#     )
#   )
# )
#
# data_fig <- bm_pred_clean |>
#   droplevels() |>
#   group_by(model, data) |>
#   summarize(
#     across(
#       .cols = c(cstat, IPA, time_fit, time_pred),
#       .fns = list(mean = mean, sd = sd)
#     )
#   ) |>
#   bind_rows(data_fig_overall) |>
#   mutate(data = factor(data, levels = data_levels)) |>
#   arrange(data, desc(IPA_mean)) |>
#   mutate(x = as.numeric(fct_rev(data))) |>
#   select(model, data, x, IPA_mean, IPA_sd) |>
#   mutate(data = recode(data, !!!data_recoder)) |>
#   group_by(data) |>
#   mutate(x = x + seq(-1/4, 1/4, length.out = n()))
#
# gg_text_data <- data_fig |>
#   group_by(data) |>
#   summarize(x = median(x)) |>
#   mutate(IPA_mean = -0.5)
#
# data_aorsf <- data_fig |>
#   filter(model == 'aorsf_cph_1')
#
# aorsf_wins <- data_fig |>
#   filter(model != 'aorsf_cph_15',
#          model != 'aorsf_net') |>
#   arrange(data, desc(IPA_mean)) |>
#   mutate(rank = seq(n())) |>
#   filter(rank == 1 & model == 'aorsf_cph_1') |>
#   pull(data)
#
# gg_text_diffs <- data_fig |>
#   filter(model != 'aorsf_cph_15',
#          model != 'aorsf_net') |>
#   arrange(data, desc(IPA_mean)) |>
#   mutate(rank = seq(n())) |>
#   filter(data %in% aorsf_wins, rank <= 2) |>
#   group_split() |>
#   map_dfr(
#     .f = ~{
#
#       tibble(data = .x$data[1],
#              pdiff = perc_diff(.x$IPA_mean[1], .x$IPA_mean[2]),
#              adiff = diff(rev(.x$IPA_mean)))
#
#     }
#   ) |>
#   left_join(gg_text_data) |>
#   mutate(
#     IPA_mean = map_dbl(
#       data,
#       ~data_aorsf |>
#         filter(data == .x) |>
#         pull(IPA_mean)
#     ),
#     label = table_glue("+{adiff} ({pdiff}%)")
#   )
#
# data_fig <- filter(data_fig,
#                    !str_detect(model, '^aorsf'),
#                    IPA_mean > 0.0)
#
# library(RColorBrewer)
#
# blues <- brewer.pal(n=length(unique(data_fig$model)), "Blues")
# standout <- "#FD8D3C"
# standout <- "purple"
#
# xmax <- max(data_fig$x)
# ymin <- .65
# ymax <- 1
#
# gg_rect <- tibble(
#   xmin = seq(xmax+1) - 1/2,
#   xmax = seq(xmax+1) + 1/2,
#   ymin = -Inf,
#   ymax = Inf
# ) |>
#   filter(seq(n()) %% 2 == 0)
#
# ggplot(data_fig) +
#   aes(x = x,
#       y = IPA_mean) +
#   geom_point(shape = 21,
#              size = 2,
#              aes(fill = model)) +
#   geom_point(data = data_aorsf,
#              shape = 21,
#              size = 3,
#              fill = standout) +
#   geom_text(data = gg_text_data, aes(label = data), hjust = 0) +
#   geom_text(data = gg_text_diffs, aes(label = label), hjust = -0.1) +
#   coord_flip() +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         legend.position = '') +
#   scale_fill_manual(values = blues) +
#   scale_y_continuous(limits = c(-.5, .75),
#                      expand = c(0,0),
#                      breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
#   geom_rect(data = gg_rect,
#             inherit.aes = FALSE,
#             aes(xmin = xmin,
#                 xmax = xmax,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = 'grey',
#             alpha = 1/6) +
#   labs(x = '', y = '')
