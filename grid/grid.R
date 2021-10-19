library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(broom)

source(here::here("functions.R"))

dark <- "#00336A"
mid <- "#004794"
light <- "#005fb8"
white <- "white"

granularity <- 250

wide_size <- wide_size
narrow_size <- wide_size / 2

set.seed(1234)

# Corner squares ----

square_start <- dark_start <- 0.5
square_end <- 3
mid_start <- square_start + wide_size
white_start <- mid_start + wide_size

bottom_left <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1, ~horizontal,
  dark_start, mid_start, square_start, square_end, dark, mid, TRUE,
  square_start, square_end, dark_start, mid_start, dark, mid, FALSE,
  mid_start, white_start, mid_start, square_end, mid, white, TRUE,
  mid_start, square_end, mid_start, white_start, mid, white, FALSE,
  white_start, square_end, white_start, square_end, white, white, FALSE
) %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal), generate_points_from_grid, granularity = granularity)) %>%
  # select(points) %>%
  unnest(cols = c(points)) %>%
  mutate(overlap = (between(x, square_start, mid_start) & between(y, square_start, mid_start)) |
    (between(x, mid_start, white_start) & between(y, mid_start, white_start))) %>%
  group_nest(overlap) %>%
  mutate(data = map2(overlap, data, function(overlap, data) {
    if (overlap) {
      data %>%
        filter((horizontal & x < y) |
          (!horizontal & x > y))
    } else {
      data
    }
  })) %>%
  select(data) %>%
  unnest(cols = data)

top_left <- bottom_left %>%
  mutate(y = -1 * y + 10)

top_right <- top_left %>%
  mutate(x = -1 * x + 10)

bottom_right <- bottom_left %>%
  mutate(x = -1 * x + 10)

# Middle perimeter ----

x_square_start <- x_dark_start <- 0.5
x_square_end <- 3
y_square_start <- y_dark_start <- 3.75
y_square_end <- 6.25

overlap <- list(
  bottom_dark = list(
    x = c(x_square_start, x_square_start + wide_size),
    y = c(y_square_start, y_square_start + narrow_size)
  ),
  top_dark = list(
    x = c(x_square_start, x_square_start + wide_size),
    y = c(y_square_end - narrow_size, y_square_end)
  ),
  bottom_light = list(
    x = c(x_square_start + wide_size, x_square_start + 2 * wide_size),
    y = c(y_square_start + narrow_size, y_square_start + wide_size)
  ),
  top_light = list(
    x = c(x_square_start + wide_size, x_square_start + 1.5),
    y = c(y_square_end - wide_size, y_square_end - narrow_size)
  )
)

overlap_line <- overlap %>%
  map(bind_rows) %>%
  imap(function(x, y) {
    if (stringr::str_starts(y, "top")) {
      x[["y"]] <- rev(x[["y"]])
      x
    } else {
      x
    }
  }) %>%
  map(function(x) {
    lm(y ~ x, data = x) %>%
      tidy() %>%
      select(term, estimate) %>%
      deframe()
  })

mid_left <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1, ~horizontal,
  x_dark_start, x_dark_start + wide_size, y_square_start, y_square_end, dark, mid, TRUE,
  x_square_start, x_square_end, y_dark_start, y_dark_start + narrow_size, dark, mid, FALSE,
  x_square_start, x_square_end, y_square_end - narrow_size, y_square_end, mid, dark, FALSE,
  x_square_start + wide_size, x_square_start + wide_size + wide_size, y_square_start + narrow_size, y_square_end - narrow_size, mid, white, TRUE,
  x_square_start + wide_size, x_square_end, y_dark_start + narrow_size, y_dark_start + wide_size, mid, white, FALSE,
  x_square_start + wide_size, x_square_end, y_square_end - wide_size, y_square_end - narrow_size, white, mid, FALSE,
  x_square_start + wide_size * 2, x_square_end, y_square_start + wide_size, y_square_end - wide_size, white, white, FALSE
) %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal), generate_points_from_grid, granularity = granularity)) %>%
  unnest(cols = c(points)) %>%
  mutate(overlap = case_when(
    between(x, overlap[["bottom_dark"]][["x"]][[1]], overlap[["bottom_dark"]][["x"]][[2]]) & between(y, overlap[["bottom_dark"]][["y"]][[1]], overlap[["bottom_dark"]][["y"]][[2]]) ~ "bottom_dark",
    between(x, overlap[["top_dark"]][["x"]][[1]], overlap[["top_dark"]][["x"]][[2]]) & between(y, overlap[["top_dark"]][["y"]][[1]], overlap[["top_dark"]][["y"]][[2]]) ~ "top_dark",
    between(x, overlap[["bottom_light"]][["x"]][[1]], overlap[["bottom_light"]][["x"]][[2]]) & between(y, overlap[["bottom_light"]][["y"]][[1]], overlap[["bottom_light"]][["y"]][[2]]) ~ "bottom_light",
    between(x, overlap[["top_light"]][["x"]][[1]], overlap[["top_light"]][["x"]][[2]]) & between(y, overlap[["top_light"]][["y"]][[1]], overlap[["top_light"]][["y"]][[2]]) ~ "top_light",
    TRUE ~ "none"
  )) %>%
  group_nest(overlap) %>%
  mutate(data = map2(overlap, data, function(overlap, data) {
    if (overlap == "none") {
      data
    } else if (stringr::str_starts(overlap, "bottom")) {
      data %>%
        filter((horizontal & y - overlap_line[[overlap]][["x"]] * x > overlap_line[[overlap]][["(Intercept)"]]) |
          (!horizontal & y - overlap_line[[overlap]][["x"]] * x < overlap_line[[overlap]][["(Intercept)"]]))
    } else if (stringr::str_starts(overlap, "top")) {
      data %>%
        filter((horizontal & y - overlap_line[[overlap]][["x"]] * x < overlap_line[[overlap]][["(Intercept)"]]) |
          (!horizontal & y - overlap_line[[overlap]][["x"]] * x > overlap_line[[overlap]][["(Intercept)"]]))
    }
  })) %>%
  select(data) %>%
  unnest(cols = data)

mid_right <- mid_left %>%
  mutate(x = -1 * x + 10)

mid_bottom <- mid_left %>%
  rename(x = y, y = x)

mid_top <- mid_right %>%
  rename(x = y, y = x)

# Center -----

size <- 0.25 * 1.5

square_start <- dark_start <- 3.75
square_end <- 6.25
mid_start <- square_start + size
white_start <- mid_start + size

overlap <- list(
  outer_bottom_left = list(
    x = c(square_start, mid_start),
    y = c(square_start, mid_start)
  ),
  outer_bottom_right = list(
    x = c(square_end - size, square_end),
    y = c(square_start, mid_start)
  ),
  inner_bottom_left = list(
    x = c(mid_start, white_start),
    y = c(mid_start, white_start)
  ),
  inner_bottom_right = list(
    x = c(square_end - size * 2, square_end - size),
    y = c(mid_start, white_start)
  ),
  outer_top_left = list(
    x = c(square_start, mid_start),
    y = c(square_end - size, square_end)
  ),
  outer_top_right = list(
    x = c(square_end - size, square_end),
    y = c(square_end - size, square_end)
  ),
  inner_top_left = list(
    x = c(mid_start, white_start),
    y = c(square_end - size * 2, square_end - size)
  ),
  inner_top_right = list(
    x = c(square_end - size * 2, square_end - size),
    y = c(square_end - size * 2, square_end - size)
  )
)

overlap_line <- overlap %>%
  map(bind_rows) %>%
  imap(function(x, y) {
    if (stringr::str_detect(y, "bottom_right") | stringr::str_detect(y, "top_left")) {
      x[["y"]] <- rev(x[["y"]])
      x
    } else {
      x
    }
  }) %>%
  map(function(x) {
    lm(y ~ x, data = x) %>%
      tidy() %>%
      select(term, estimate) %>%
      deframe()
  })

center <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1, ~horizontal,
  dark_start, mid_start, square_start, square_end, dark, mid, TRUE,
  square_start, square_end, dark_start, mid_start, dark, mid, FALSE,
  square_end - size, square_end, square_start, square_end, mid, dark, TRUE,
  square_start, square_end, square_end - size, square_end, mid, dark, FALSE,
  mid_start, white_start, mid_start, square_end - size, mid, white, TRUE,
  square_end - size * 2, square_end - size, mid_start, square_end - size, white, mid, TRUE,
  mid_start, square_end - size, mid_start, white_start, mid, white, FALSE,
  mid_start, square_end - size, square_end - size * 2, square_end - size, white, mid, FALSE,
  mid_start + size, square_end - size * 2, mid_start + size, square_end - size * 2, white, white, FALSE
) %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal), generate_points_from_grid, granularity = granularity)) %>%
  # select(points) %>%
  unnest(cols = c(points)) %>%
  mutate(overlap = case_when(
    between(x, overlap[["outer_bottom_left"]][["x"]][[1]], overlap[["outer_bottom_left"]][["x"]][[2]]) & between(y, overlap[["outer_bottom_left"]][["y"]][[1]], overlap[["outer_bottom_left"]][["y"]][[2]]) ~ "outer_bottom_left",
    between(x, overlap[["outer_bottom_right"]][["x"]][[1]], overlap[["outer_bottom_right"]][["x"]][[2]]) & between(y, overlap[["outer_bottom_right"]][["y"]][[1]], overlap[["outer_bottom_right"]][["y"]][[2]]) ~ "outer_bottom_right",
    between(x, overlap[["inner_bottom_left"]][["x"]][[1]], overlap[["inner_bottom_left"]][["x"]][[2]]) & between(y, overlap[["inner_bottom_left"]][["y"]][[1]], overlap[["inner_bottom_left"]][["y"]][[2]]) ~ "inner_bottom_left",
    between(x, overlap[["inner_bottom_right"]][["x"]][[1]], overlap[["inner_bottom_right"]][["x"]][[2]]) & between(y, overlap[["inner_bottom_right"]][["y"]][[1]], overlap[["inner_bottom_right"]][["y"]][[2]]) ~ "inner_bottom_right",
    between(x, overlap[["outer_top_left"]][["x"]][[1]], overlap[["outer_top_left"]][["x"]][[2]]) & between(y, overlap[["outer_top_left"]][["y"]][[1]], overlap[["outer_top_left"]][["y"]][[2]]) ~ "outer_top_left",
    between(x, overlap[["outer_top_right"]][["x"]][[1]], overlap[["outer_top_right"]][["x"]][[2]]) & between(y, overlap[["outer_top_right"]][["y"]][[1]], overlap[["outer_top_right"]][["y"]][[2]]) ~ "outer_top_right",
    between(x, overlap[["inner_top_left"]][["x"]][[1]], overlap[["inner_top_left"]][["x"]][[2]]) & between(y, overlap[["inner_top_left"]][["y"]][[1]], overlap[["inner_top_left"]][["y"]][[2]]) ~ "inner_top_left",
    between(x, overlap[["inner_top_right"]][["x"]][[1]], overlap[["inner_top_right"]][["x"]][[2]]) & between(y, overlap[["inner_top_right"]][["y"]][[1]], overlap[["inner_top_right"]][["y"]][[2]]) ~ "inner_top_right",
    TRUE ~ "none"
  )) %>%
  group_nest(overlap) %>%
  mutate(data = map2(overlap, data, function(overlap, data) {
    if (stringr::str_detect(overlap, "top_right")) {
      data <- data %>%
        mutate(keep = (horizontal & y - overlap_line[[overlap]][["x"]] * x < overlap_line[[overlap]][["(Intercept)"]]) |
          (!horizontal & y - overlap_line[[overlap]][["x"]] * x > overlap_line[[overlap]][["(Intercept)"]]))

      data %>%
        filter(keep)
    } else if (stringr::str_detect(overlap, "top_left")) {
      data <- data %>%
        mutate(keep = (horizontal & y - overlap_line[[overlap]][["x"]] * x < overlap_line[[overlap]][["(Intercept)"]]) |
          (!horizontal & y - overlap_line[[overlap]][["x"]] * x > overlap_line[[overlap]][["(Intercept)"]]))

      data %>%
        filter(keep)
    } else if (stringr::str_detect(overlap, "bottom_left")) {
      data <- data %>%
        mutate(keep = (!horizontal & y - overlap_line[[overlap]][["x"]] * x < overlap_line[[overlap]][["(Intercept)"]]) |
          (horizontal & y - overlap_line[[overlap]][["x"]] * x > overlap_line[[overlap]][["(Intercept)"]]))

      data %>%
        filter(keep)
    } else if (stringr::str_detect(overlap, "bottom_right")) {
      data <- data %>%
        mutate(keep = (!horizontal & y - overlap_line[[overlap]][["x"]] * x < overlap_line[[overlap]][["(Intercept)"]]) |
          (horizontal & y - overlap_line[[overlap]][["x"]] * x > overlap_line[[overlap]][["(Intercept)"]]))

      data %>%
        filter(keep)
    } else {
      data
    }
  })) %>%
  # select(data) %>%
  unnest(cols = data)

# Background ----

background <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1, ~horizontal,
  0, 10, 0, 10, dark, mid, TRUE,
) %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2, horizontal), generate_points_from_grid, granularity = granularity * 2 / 3)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Combine ----

grid <- background %>%
  bind_rows(bottom_left) %>%
  bind_rows(top_left) %>%
  bind_rows(top_right) %>%
  bind_rows(bottom_right) %>%
  bind_rows(mid_left) %>%
  bind_rows(mid_right) %>%
  bind_rows(mid_top) %>%
  bind_rows(mid_bottom) %>%
  bind_rows(center)

p <- grid %>%
  ggplot(aes(x = x, y = y, color = color)) +
  geom_point(size = 0.1, shape = 15) +
  scale_color_identity() +
  theme_void() +
  coord_fixed()

p

ggsave(here::here("grid/grid.png"), p, width = 15, height = 15)
