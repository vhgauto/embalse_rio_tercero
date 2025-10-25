ggplot() +
  geom_spatraster_rgb(
    data = r,
    r = 4,
    g = 3,
    b = 2,
    max_col_value = .3,
    interpolate = FALSE,
    # stretch = "hist"
  ) +
  theme_void()

ggplot() +
  geom_spatraster(
    data = predict_ds
  ) +
  theme_void()
