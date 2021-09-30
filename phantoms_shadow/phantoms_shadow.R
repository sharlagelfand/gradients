library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

source(here::here("functions.R"))

yellow <- "#FFAE00"
blue <- "#0053AA"
turquoise <- "#008F9D"
pink <- "#F58671"

set.seed(1234)

layout <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1,

  0, 1, 0, 1, turquoise, turquoise,
  0, 1, 1, 2, yellow, yellow,
  0, 1, 2, 3, pink, pink,
  0, 1, 3, 4, blue, blue,
  0, 1, 4, 5, pink, pink,
  0, 1, 5, 7, turquoise, blue,
  0, 1, 7, 8, yellow, yellow,

  1, 3, 0, 1, pink, yellow,
  1, 2, 1, 3, turquoise, blue,
  1, 3, 3, 4, yellow, blue,
  1, 2, 4, 5, blue, blue,
  1, 3, 5, 6, yellow, pink,
  1, 3, 6, 7, pink, turquoise,
  1, 2, 7, 8, turquoise, turquoise,

  2, 3, 1, 3, turquoise, yellow,
  2, 3, 4, 5, turquoise, turquoise,
  2, 3, 7, 8, yellow, yellow,

  3, 4, 0, 1, blue, blue,
  3, 4, 1, 2, pink, pink,
  3, 4, 2, 3, turquoise, turquoise,
  3, 4, 3, 4, pink, pink,
  3, 4, 4, 5, yellow, yellow,
  3, 4, 5, 7, blue, pink,
  3, 4, 7, 8, blue, blue,

  4, 5, 0, 1, pink, pink,
  4, 5, 1, 3, turquoise, blue,
  4, 5, 3, 4, yellow, yellow,
  4, 5, 4, 5, blue, blue,
  4, 5, 5, 6, pink, pink,
  4, 5, 6, 7, yellow, yellow,
  4, 5, 7, 8, turquoise, turquoise,

  5, 6, 0, 1, blue, blue,
  5, 7, 1, 2, yellow, pink,
  5, 7, 2, 3, pink, turquoise,
  5, 6, 3, 4, turquoise, turquoise,
  5, 7, 4, 5, yellow, pink,
  5, 6, 5, 7, turquoise, blue,
  5, 7, 7, 8, yellow, turquoise,

  6, 7, 0, 1, turquoise, turquoise,
  6, 7, 3, 4, yellow, yellow,
  6, 7, 5, 7, blue, pink,

  7, 8, 0, 1, yellow, yellow,
  7, 8, 1, 3, blue, pink,
  7, 8, 3, 4, blue, blue,
  7, 8, 4, 5, turquoise, turquoise,
  7, 8, 5, 6, yellow, yellow,
  7, 8, 6, 7, blue, blue,
  7, 8, 7, 8, yellow, yellow

)

gradient_grid <- layout %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2), generate_points_from_grid, granularity = 500)) %>%
  select(points) %>%
  unnest(cols = c(points))

p <- gradient_grid %>%
  ggplot(aes(x = x, y = y, color = color)) +
  geom_point(size = 0.01, shape = 15) +
  scale_color_identity() +
  theme_void() +
  coord_fixed()

ggsave(here::here("phantoms_shadow", "phantoms_shadow.png"), plot = p, width = 10, height = 10, dpi = 300)
