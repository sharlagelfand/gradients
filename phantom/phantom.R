library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

brown <- "#371022"
pink <- "#EC8772"
orange <- "#F1711B"
red <- "#E43700"
yellow <- "#F4DB49"
gold <- "#FFAC00"
maroon <- "#901824"
blue <- "#008B9A"

set.seed(1234)

layout <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1,

  0, 1, 0, 1, brown, brown,
  0, 1, 1, 3, pink, red,
  0, 1, 3, 4, yellow, yellow,
  0, 1, 4, 5, pink, pink,
  0, 1, 5, 7, gold, maroon,
  0, 1, 7, 8, red, red,

  1, 2, 0, 1, blue, blue,
  1, 3, 1, 2, gold, red,
  1, 3, 2, 3, maroon, gold,
  1, 2, 3, 5, orange, brown,
  1, 3, 5, 6, blue, maroon,
  1, 3, 6, 7, orange, blue,
  1, 2, 7, 8, yellow, yellow,

  2, 3, 0, 1, maroon, maroon,
  2, 3, 3, 5, blue, orange,
  2, 3, 7, 8, brown, brown,

  3, 5, 0, 1, orange, blue,
  3, 4, 1, 3, yellow, pink,
  3, 5, 3, 4, brown, orange,
  3, 5, 4, 5, yellow, brown,
  3, 4, 5, 7, red, gold,
  3, 5, 7, 8, pink, yellow,

  4, 5, 1, 3, brown, yellow,
  4, 5, 5, 7, pink, red,

  5, 6, 0, 1, gold, gold,
  5, 7, 1, 2, pink, yellow,
  5, 7, 2, 3, red, pink,
  5, 6, 3, 5, maroon, blue,
  5, 7, 5, 6, gold, orange,
  5, 7, 6, 7, maroon, gold,
  5, 6, 7, 8, orange, orange,

  6, 7, 0, 1, red, red,
  6, 7, 3, 5, gold, maroon,
  6, 7, 7, 8, blue, blue,

  7, 8, 0, 1, maroon, maroon,
  7, 8, 1, 3, orange, brown,
  7, 8, 3, 4, blue, blue,
  7, 8, 4, 5, orange, orange,
  7, 8, 5, 7, yellow, pink,
  7, 8, 7, 8, brown, brown
)

gradient_grid <- layout %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2), generate_points_from_grid, granularity = 500)) %>%
  select(points) %>%
  unnest(cols = c(points))

p <- gradient_grid %>%
  ggplot(aes(x = x, y = y, color = color)) +
  geom_point(size = 0.1, shape = 15) +
  scale_color_identity() +
  theme_void() +
  coord_fixed()

ggsave(here::here("phantom", "phantom.png"), plot = p, width = 10, height = 10, dpi = 300)
