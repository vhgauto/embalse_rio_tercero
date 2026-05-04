# datos ------------------------------------------------------------------

d_ciano <- filter(d, param == "cla_ciano") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ciano_m = mean(valor, na.rm = TRUE),
    ciano_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  ) |>
  mutate(fecha = update(fecha, day = 1))

# figura -----------------------------------------------------------------

g_ciano_año <- ggplot(d_ciano, aes(fecha, ciano_m)) +
  geom_segment(
    aes(color = "a"),
    x = fecha_i,
    xend = fecha_f,
    y = filter(promedio_tbl, param == "cla_ciano")$m,
    yend = filter(promedio_tbl, param == "cla_ciano")$m,
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
    aes(ymin = ciano_m - ciano_sd, ymax = ciano_m + ciano_sd, color = "b"),
    linewidth = .25,
    width = 2,
    key_glyph = "crossbar"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    linewidth = .4,
    color = "black",
    span = .2
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
    expand = expansion(mult = 0, add = c(0, 0))
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
  labs(
    y = "Concentración de clorofila-a<br>de cianobacterias (mg/m<sup>3</sup>)",
    x = NULL,
    color = NULL
  ) +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    plot.margin = margin(20, 5, 5, 5),
    legend.position = "bottom",
    legend.box.spacing = unit(0, "mm"),
    legend.key.spacing.x = unit(10, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    axis.text = element_text(color = "black"),
    axis.title.y = ggtext::element_markdown(),
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
  plot = g_ciano_año,
  filename = "figura_ciano_año",
  ancho = 5,
  alto = 3
)

if (FALSE) {
  browseURL(paste0(
    "fig/",
    año_actual,
    "-",
    mes_actual_chr,
    "/figura_ciano_año_",
    año_actual,
    "_",
    mes_actual_chr,
    ".png"
  ))
}
