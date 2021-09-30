generate_gradient_colours <- function(colour_1, colour_2, size, probability) {
  sample(
    c(colour_1, colour_2),
    size = size,
    replace = TRUE,
    prob = c(probability, 1 - probability)
  )
}

generate_points_from_grid <- function(xmin, xmax, ymin, ymax, colour_1, colour_2, granularity = 100, horizontal = NULL) {
  x_size <- xmax - xmin
  y_size <- ymax - ymin

  grid <- expand.grid(
    x = seq(xmin, xmax, length.out = granularity * x_size),
    y = seq(ymin, ymax, length.out = granularity * y_size)
  ) %>%
    as_tibble() %>%
    filter(!x %in% c(xmin, xmax)) %>%
    filter(!y %in% c(ymin, ymax))

  if (colour_1 == colour_2) {
    grid <- grid %>%
      mutate(color = colour_1)

    return(grid)
  }

  if (is.null(horizontal)) {
    horizontal_gradient <- x_size > y_size
  } else {
    horizontal_gradient <- horizontal
  }

  if (horizontal_gradient) {
    grid <- grid %>%
      mutate(prop = (x - xmin) / x_size)

    sample_size <- grid %>%
      distinct(y) %>%
      nrow()
  } else {
    grid <- grid %>%
      mutate(prop = (y - ymin) / y_size)

    sample_size <- grid %>%
      distinct(x) %>%
      nrow()
  }

  grid %>%
    group_nest(prop) %>%
    mutate(color = map(
      prop,
      ~ generate_gradient_colours(
        colour_1 = colour_1,
        colour_2 = colour_2,
        size = sample_size,
        probability = .x
      )
    )) %>%
    unnest(cols = c(data, color))
}
