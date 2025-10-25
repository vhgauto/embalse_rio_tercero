# datos ------------------------------------------------------------------

d_temp <- filter(d, param == "temperatura") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    temp_m = median(valor, na.rm = TRUE),
    temp_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

# figura -----------------------------------------------------------------

g_temp <- ggplot(d_temp, aes(fecha, temp_m)) +
  geom_segment(
    aes(color = "a"),
    x = fecha_i,
    xend = fecha_f,
    y = filter(promedio_tbl, param == "temperatura")$m,
    yend = filter(promedio_tbl, param == "temperatura")$m,
    linetype = "22",
    linewidth = .5
  ) +
  geom_rect(
    data = d_est,
    aes(
      xmin = fecha_min,
      xmax = fecha_max,
      ymin = I(1.05),
      ymax = I(1),
      fill = fill
    ),
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    aes(ymin = temp_m - temp_sd, ymax = temp_m + temp_sd, color = "b"),
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
    breaks = scales::breaks_width(5),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_color_manual(
    breaks = c("b", "a"),
    values = c("black", "dodgerblue"),
    labels = c("Media mensual", "Media período 2015-2024")
  ) +
  scale_fill_identity() +
  coord_cartesian(clip = "off") +
  labs(y = "Temperatura del agua (°C)", x = NULL, color = NULL) +
  theme_bw(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(10, 5, 5, 5),
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
  )

# guardo -----------------------------------------------------------------

guardar_png(
  plot = g_temp,
  filename = "figura_temperatura",
  ancho = 5,
  alto = 3
)

# promedios y desvíos ----------------------------------------------------

temp0_tbl <- filter(
  d_temp,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

temp1_tbl <- filter(
  d_temp,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

temp2_tbl <- filter(
  d_temp,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

temp0 <- formato(temp0_tbl$temp_m)
temp0_sd <- formato(temp0_tbl$temp_sd)

temp1 <- formato(temp1_tbl$temp_m)
temp1_sd <- formato(temp1_tbl$temp_sd)

temp2 <- formato(temp2_tbl$temp_m)
temp2_sd <- formato(temp2_tbl$temp_sd)

temp_p <- formato(filter(promedio_tbl, param == "temperatura")$m)
temp_p_sd <- formato(filter(promedio_tbl, param == "temperatura")$sd)
