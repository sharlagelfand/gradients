library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

source(here::here("functions.R"))

set.seed(1234)

"#4BE345" -> green
"#ff1200" -> red

large <- 0.5
mid <- 0.35
narrow <- 0.1

layout <- tribble(
  ~size, ~colour_2, ~colour_1, ~horizontal,
  large, red, green, FALSE,
  mid, green, red, FALSE,
  large, green, red, TRUE,
  large, green, red, TRUE,
  narrow, red, green, FALSE,
  large, green, red, FALSE,
  large, red, green, TRUE,
  large, red, green, FALSE,
  large, green, red, TRUE,
  mid, red, green, FALSE,
  large, red, green, TRUE,
  large, green, red, FALSE,
  narrow, green, red, TRUE,
  mid, red, green, TRUE,
  mid, red, green, TRUE
) %>%
  mutate(
    xmax = cumsum(size),
    xmin = xmax - size,
    ymin = 0, ymax = 1
  )

gradient_grid <- layout %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal), generate_points_from_grid, granularity = 500)) %>%
  select(points) %>%
  unnest(cols = c(points))

p <- ggplot() +
  geom_rect(aes(xmin = -0.4, xmax = 6.5, ymin = -1, ymax = 2), fill = red, color = red) +
  geom_point(data = gradient_grid, aes(x = x, y = y, color = color), size = 0.01, shape = 15) +
  scale_color_identity() +
  theme_void() +
  coord_fixed(xlim = c(-0.4, 6.5), ylim = c(-1, 2), expand = FALSE)

ggsave(here::here("live_wire", "live_wire.png"), width = 12, height = 6, dpi = 300)
