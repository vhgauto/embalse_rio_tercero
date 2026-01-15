# datos ------------------------------------------------------------------

d_ds <- filter(d, param == "ds") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ds_m = mean(valor, na.rm = TRUE),
    ds_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

# figura -----------------------------------------------------------------

g_ds <- ggplot(d_ds, aes(fecha, ds_m)) +
  geom_segment(
    aes(color = "a"),
    x = fecha_i,
    xend = fecha_f,
    y = filter(promedio_tbl, param == "ds")$m,
    yend = filter(promedio_tbl, param == "ds")$m,
    linetype = "22",
    linewidth = .5
  ) +
  geom_rect(
    data = d_est,
    aes(
      xmin = fecha_min,
      xmax = fecha_max,
      ymin = I(altura_estacion_label),
      ymax = I(1),
      fill = fill
    ),
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    aes(ymin = ds_m - ds_sd, ymax = ds_m + ds_sd, color = "b"),
    linewidth = .25,
    width = 2,
    key_glyph = "crossbar"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    linewidth = .4,
    color = "black",
    span = .2,
    se = FALSE
  ) +
  geom_point(
    aes(color = "b"),
    size = 1,
    shape = 21,
    fill = "white",
    stroke = 1
  ) +
  geom_text(
    data = d_est,
    aes(fecha_label, I(1.01), label = estacion_label),
    inherit.aes = FALSE,
    size = tamaño_label_est,
    vjust = -.2,
    fontface = "bold"
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 month"),
    labels = \(x) str_to_sentence(format(x, "%b '%y")),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05), add = c(0, 0)),
    breaks = scales::breaks_width(1),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_color_manual(
    breaks = c("b", "a"),
    values = c("black", "dodgerblue"),
    labels = c("Media mensual", "Media período 2015-2024")
  ) +
  scale_fill_identity() +
  coord_cartesian(clip = "off") +
  labs(y = "Transparencia del agua (m)", x = NULL, color = NULL) +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    plot.margin = margin(11, 5, 5, 5),
    legend.position = "bottom",
    legend.box.spacing = unit(0, "mm"),
    legend.key.spacing.x = unit(10, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    axis.text = element_text(color = "black"),
    axis.ticks.x = element_line(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x = unit(14, "pt"),
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.clip = "off"
  ) +
  theme_sub_axis_x(text = element_text(angle = 45, hjust = 1)) +
  theme_sub_legend(
    text = element_text(margin = margin(r = 2), size = rel(tamaño_texto_legend))
  )

# guardo -----------------------------------------------------------------

guardar_png(
  plot = g_ds,
  filename = "figura_transparencia",
  ancho = 5,
  alto = 3
)

# promedios y desvíos ----------------------------------------------------

ds0_tbl <- filter(
  d_ds,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

ds1_tbl <- filter(
  d_ds,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

ds2_tbl <- filter(
  d_ds,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

ds0 <- formato(ds0_tbl$ds_m)
ds0_sd <- formato(ds0_tbl$ds_sd)

ds1 <- formato(ds1_tbl$ds_m)
ds1_sd <- formato(ds1_tbl$ds_sd)

ds2 <- formato(ds2_tbl$ds_m)
ds2_sd <- formato(ds2_tbl$ds_sd)

ds_p <- formato(filter(promedio_tbl, param == "dseratura")$m)
ds_p_sd <- formato(filter(promedio_tbl, param == "dseratura")$sd)
