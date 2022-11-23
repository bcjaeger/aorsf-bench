set.seed(329)

poly_1 <- tibble(
  x = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0),
  y = c(0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1),
  f = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  ff = factor(f, labels = c(': Original',
                            ': Negate y coefficient',
                            ': Negate x coefficient'))
)

poly_2 = poly_1 %>%
  mutate(x = -(x-1), y = -(y-1))

points <- tibble(
  x = runif(100),
  y = runif(100)
)

points_1 <- points %>%
  filter(x < y)

points_2 <- points %>%
  filter(x > y)

plot_ly(x = ~x, y = ~y) %>%
  add_polygons(data = poly_1,
               frame = ~paste0(f, ff),
               color = I('purple'),
               alpha = I(1/5)) %>%
  add_polygons(data = poly_2,
               frame = ~paste0(f, ff),
               color = I('orange'),
               alpha = I(1/5)) %>%
  add_markers(data = points_1,
              color = I("purple")) %>%
  add_markers(data = points_2,
              color = I("orange")) %>%
  animation_opts(frame = 1000,
                 transition = 1000,
                 easing = 'elastic',
                 redraw = FALSE) %>%
  animation_slider(currentvalue = list(prefix = "")) %>%
  hide_legend()




poly_2 <- tibble(
  x = c(-2, -2, 2, -2, -2, -2, 2, -2, -2, 2, 2, -2),
  y = c(-2, 2, 2, -2, -2, 2, -2, -2, 2, -2, 2, 2),
  f = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
  ff = factor(f, labels = c(': Original',
                            ': Negate y coefficient',
                            ': Negate x coefficient'))
)

poly_2 = poly_1 %>%
  mutate(x = -x, y = -y)

points <- tibble(
  x = rnorm(100),
  y = x + rnorm(100, sd=1/2)
) %>%
  filter(x > -2, x < 2,
         y > -2, y < 2)

points_1 <- points %>%
  filter(y < x)

points_2 <- points %>%
  filter(y > x)

plot_ly(x = ~x, y = ~y) %>%
  add_polygons(data = poly_1,
               frame = ~paste0(f, ff),
               color = I('purple'),
               alpha = I(1/5)) %>%
  add_polygons(data = poly_2,
               frame = ~paste0(f, ff),
               color = I('orange'),
               alpha = I(1/5)) %>%
  add_markers(data = points_1,
              color = I("purple")) %>%
  add_markers(data = points_2,
              color = I("orange")) %>%
  animation_opts(frame = 1000,
                 transition = 1000,
                 easing = 'elastic',
                 redraw = FALSE) %>%
  animation_slider(currentvalue = list(prefix = "")) %>%
  hide_legend()


poly_a = tibble(
  x = c(-2, -2, 1/2, -1/2, -2),
  y = c(-2,  2, 2, -2, -2),
  f = c( 1,  1, 1,  1,  1)
)

poly_b = mutate(poly_a, y = -y, f = 2)
poly_c = mutate(poly_a, x = -x, f = 3)

poly_1 <- bind_rows(poly_a, poly_b, poly_c) %>%
  mutate(ff = factor(f, labels = c(': Original',
                              ': Negate y coefficient',
                              ': Negate x coefficient')))

poly_2 = poly_1 %>%
  mutate(x = -x, y = -y)

points <- tibble(
  x = rnorm(250),
  y = x + rnorm(250, sd=1/2)
) %>%
  filter(x > -2, x < 2,
         y > -2, y < 2)

points_1 <- points %>%
  filter(y > 4*x)

points_2 <- points %>%
  filter(y < 4*x)

plot_ly(x = ~x, y = ~y) %>%
  add_polygons(data = poly_1,
               frame = ~paste0(f, ff),
               color = I('purple'),
               alpha = I(1/5)) %>%
  add_polygons(data = poly_2,
               frame = ~paste0(f, ff),
               color = I('orange'),
               alpha = I(1/5)) %>%
  add_markers(data = points_1,
              color = I("purple")) %>%
  add_markers(data = points_2,
              color = I("orange")) %>%
  animation_opts(frame = 1000,
                 transition = 1000,
                 easing = 'elastic',
                 redraw = FALSE) %>%
  # add_annotations()
  animation_slider(currentvalue = list(prefix = "")) %>%
  hide_legend() %>%
  layout(
    # title = "Original decision boundary: 4x - y > 0",
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    font  = list(size = 14)
  )



# first boundary:
# y = 2x + 0

# second boundary:
# y = -2x + 1
# => 4x +y <= 0 means predict purple

poly_a = tibble(
  x = c(-2, -2, 1/2, -1/2, -2),
  y = c(-2,  2, 2, -2, -2),
) %>%
  mutate(f = 1, color = 'purple1')

poly_b = tibble(
  x = c(-1/2, 1/4, 1, -1/2),
  y = c(-2,  1, -2, -2)
) %>%
  mutate(f = 1, color = 'orange1')

poly_c = tibble(
  x = c(1/4, 1/2, 2,  2,  1, 1/4),
  y = c(1,   2,   2, -2, -2, 1)
) %>%
  mutate(f = 1, color = 'purple2')

poly_a_flip_y = mutate(poly_a, y = -y, f = f + 1)

poly_a_flip_x = tibble(
  x = c(),
  y = c()
) %>%
  mutate(frame = 3)

poly_b_flip_y = mutate(poly_b, y = -y, f = f + 1)
poly_b_flip_x = mutate(poly_b, y = -y, color = 'purple1', f = f + 2)

poly_c_flip_y = mutate(poly_c, y = -y, f = f + 1)
poly_c_flip_x = mutate(poly_c, y = -y, color = 'orange2', f = f + 2)

poly_data <- bind_rows(poly_a,
                       poly_b,
                       poly_c,
                       poly_a_flip_y,
                       poly_b_flip_y,
                       poly_c_flip_y,
                       poly_a_flip_x,
                       poly_b_flip_x,
                       poly_c_flip_x
                       ) %>%
  mutate(ff = factor(f, levels = 1:3,
                     labels = c(': Original tree',
                                ': Negate y coefficient',
                                ': Negate x coefficient')))

points <- tibble(
  x = rnorm(250),
  y = x + rnorm(250, sd=1/2)
)

points_orange <- points %>%
  filter(y < 4*x & y < -4*x + 2)

points_purple <- points %>%
  filter(!(y < 4*x & y < -4*x + 2))

plot_ly(x = ~x, y = ~y, colors = c("orange", "orange", "purple", "purple")) %>%
  add_polygons(data = poly_data,
               frame = ~paste0(f, ff),
               color = ~ color,
               alpha = I(1/5)) %>%
  add_markers(data = points_purple,
              color = I("purple")) %>%
  add_markers(data = points_orange,
              color = I("orange")) %>%
  animation_opts(frame = 1000,
                 transition = 1000,
                 easing = 'elastic') %>%
  # add_annotations()
  animation_slider(currentvalue = list(prefix = "")) %>%
  hide_legend() %>%
  layout(
    # title = "Original decision boundary: 4x - y > 0",
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    font  = list(size = 14)
  )



