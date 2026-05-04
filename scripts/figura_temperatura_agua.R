# https://grafana.ohmc.ar/d/U8oH3kKVz/eml03-embalse-rio-3
# https://grafana.ohmc.ar/d/dr_IMbqWz/eml02-dique-los-molinos

# datos ------------------------------------------------------------------

d_temp <- filter(d, param == "temperatura") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    temp_m = mean(valor, na.rm = TRUE),
    temp_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  ) |>
  mutate(fecha = update(fecha, day = 1))

d_atm <- read_csv(
  # list.files("datos/temperatura/", full.names = TRUE),
  "datos/temperatura/Temperatura del aire-data-2026-03-02 15_18_44.csv",
  show_col_types = FALSE
) |>
  distinct() |>
  rename(fecha = 1, temp = 2) |>
  mutate(fecha = as.Date(fecha)) |>
  mutate(temp = sub(" °C", "", temp)) |>
  mutate(temp = as.numeric(temp)) |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  reframe(temp = mean(temp), .by = fecha) |>
  filter(temp != 0)

# si hay fechas sin registro de datos de temperatura, queda el espacio vacío
d_atm_na <- tibble(
  fecha = seq.Date(min(d_atm$fecha), max(d_atm$fecha), by = "1 day")
) |>
  full_join(d_atm, by = join_by(fecha))

# figura -----------------------------------------------------------------

g_temp <- ggplot(d_temp, aes(fecha, temp_m)) +
  geom_line(
    data = d_atm_na,
    aes(fecha, temp, linetype = "Temp. ambiente"),
    inherit.aes = FALSE,
    color = "#02AF4D",
    linewidth = .4
  ) +
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
      ymin = I(altura_estacion_label),
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
  scale_linetype_manual(
    name = NULL,
    values = 1,
    guide = guide_legend(
      override.aes = list(linewidth = .7),
      theme = theme_sub_legend(margin = margin(l = 20))
    )
  ) +
  coord_cartesian(clip = "off") +
  labs(y = "Temperatura del agua (°C)", x = NULL, color = NULL) +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    plot.margin = margin(11, 10, 5, 5),
    legend.position = "bottom",
    legend.box.spacing = unit(0, "mm"),
    legend.key.spacing.x = unit(5, "mm"),
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
  plot = g_temp,
  filename = "figura_temperatura",
  ancho = 6,
  alto = 3.5
)

if (FALSE) {
  browseURL(paste0(
    "fig/",
    año_actual,
    "-",
    mes_actual_chr,
    "/figura_temperatura_",
    año_actual,
    "_",
    mes_actual_chr,
    ".png"
  ))
}
