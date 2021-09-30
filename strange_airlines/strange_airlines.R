library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

source(here::here("functions.R"))

set.seed(1234)

dark_blue <- "#418CCF"
sky_blue <- "#82B8D8"
light_blue <- "#D1E5EC"

increment <- 0.5

layout <- expand.grid(ymin = 0, ymax = 5, xmin = seq(0, 9, by = increment)) %>%
  mutate(
    id = row_number(),
    xmax = xmin + increment,
    colour_1 = ifelse(id %% 2 == 0, sky_blue, dark_blue),
    colour_2 = ifelse(id %% 2 == 0, dark_blue, sky_blue)
  )

gradient_grid <- layout %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2), generate_points_from_grid, granularity = 200)) %>%
  select(points) %>%
  unnest(cols = c(points))

p <- gradient_grid %>%
  ggplot(aes(x = x, y = y, color = color)) +
  geom_point(size = 0.1, shape = 15) +
  scale_color_identity() +
  theme_void() +
  coord_fixed()

ggsave(here::here("strange_airlines", "strange_airlines.png"), width = 9, height = 5, dpi = 300)
