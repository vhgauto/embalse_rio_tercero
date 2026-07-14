ggplot2::ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = r,
    r = 4,
    g = 3,
    b = 2,
    max_col_value = .03,
    interpolate = FALSE,
    # stretch = "hist"
  ) +
  theme_void()
