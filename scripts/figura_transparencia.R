d_ds <- read_csv("datos/d.csv", show_col_types = FALSE) |>
  filter(param == "ds") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  mutate(fecha_label = str_to_sentence(format(fecha, "%b %y"))) |>
  mutate(fecha_label = fct_reorder(as.character(fecha_label), fecha)) |>
  drop_na(valor)

d_ds_m <- d_ds |>
  reframe(
    m = median(valor),
    .by = fecha
  )

# d_est <- expand_grid(
#   mes = c(3, 6, 9, 12),
#   año = c(año_actual, año_actual - 1)
# ) |>
#   mutate(
#     estacion = case_match(
#       mes,
#       3 + 3 ~ "otoño",
#       6 + 3 ~ "invierno",
#       9 + 3 ~ "primavera",
#       3 ~ "verano"
#     )
#   ) |>
#   mutate(fecha = make_date(year = año, month = mes, day = 21)) |>
#   arrange(fecha) |>
#   filter(between(fecha, fecha_i, fecha_f)) |>
#   mutate(fecha_min = lag(fecha, default = min(d_ds$fecha))) |>
#   mutate(fecha_max = if_else(fecha == min(fecha), min(d_ds$fecha), fecha)) |>
#   mutate(fecha_max = if_else(fecha_max == min(fecha), fecha, fecha)) |>
#   rowwise() |>
#   mutate(fecha_label = fecha_min + (fecha_max - fecha_min) / 2) |>
#   ungroup() |>
#   mutate(
#     estacion_label = if_else(row_number() == 1, "", str_to_sentence(estacion))
#   ) |>
#   mutate(fill = color_estaciones[estacion])

g_ds <- ggplot(d_ds, aes(fecha, valor)) +
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
  stat_summary(
    geom = "errorbar",
    fun = "median",
    fun.min = \(x) median(x) - sd(x),
    fun.max = \(x) median(x) + sd(x),
    linewidth = .25,
    width = 2
  ) +
  geom_smooth(
    data = d_ds_m,
    aes(fecha, m),
    inherit.aes = FALSE,
    method = "loess",
    formula = y ~ x,
    color = "black",
    linewidth = .4,
    span = .2
  ) +
  stat_summary(
    geom = "point",
    fun = median,
    color = "black",
    size = 1,
    shape = 21,
    fill = "white",
    stroke = 1
  ) +
  geom_vline(xintercept = d_est$fecha, linetype = 2, linewidth = .2) +
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
    labels = \(x) str_to_sentence(format(x, "%b %y")),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05), add = c(0, 0)),
    breaks = scales::breaks_width(5),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_fill_identity() +
  coord_cartesian(clip = "off") +
  labs(y = "Transparencia del agua (m)", x = NULL) +
  theme_bw(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(10, 5, 5, 5),
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.ticks.x = element_line(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x = unit(14, "pt"),
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.clip = "off"
  )

guardar_png(
  plot = g_ds,
  filename = paste0(
    carpeta_fig,
    "/figura_transparencia_",
    año_actual,
    "_",
    mes_actual,
    ".png"
  ),
  ancho = 5,
  alto = 3
)
