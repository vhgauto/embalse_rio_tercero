d_ds <- read_csv("datos/d.csv", show_col_types = FALSE) |>
  filter(param == "ds") |>
  select(fecha, valor) |>
  drop_na() |>
  mutate(mes = month(fecha), año = year(fecha)) |>
  reframe(ds = mean(valor), .by = c(año, mes)) |>
  mutate(fecha = make_date(year = año, month = mes, day = 21)) |>
  filter(between(fecha, fecha_i, fecha_f))

d_cla <- read_csv("datos/d.csv", show_col_types = FALSE) |>
  filter(param == "cla") |>
  select(fecha, valor) |>
  drop_na() |>
  mutate(mes = month(fecha), año = year(fecha)) |>
  reframe(cla = mean(valor), .by = c(año, mes)) |>
  mutate(fecha = make_date(year = año, month = mes, day = 1)) |>
  filter(between(fecha, fecha_i, fecha_f))

d_tsi <- inner_join(d_ds, d_cla, by = join_by(año, mes)) |>
  select(-fecha.x) |>
  rename(fecha = fecha.y) |>
  mutate(
    tsi_ds = 60 - 14.41 * log(ds),
    tsi_cla = 30.6 + 9.81 * log(cla)
  ) |>
  mutate(tsi = (tsi_ds + tsi_cla) / 2)

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
#   mutate(fecha_min = lag(fecha, default = min(d_tsi$fecha))) |>
#   mutate(fecha_max = if_else(fecha == min(fecha), min(d_tsi$fecha), fecha)) |>
#   mutate(fecha_max = if_else(fecha_max == min(fecha), fecha, fecha)) |>
#   rowwise() |>
#   mutate(fecha_label = fecha_min + (fecha_max - fecha_min) / 2) |>
#   ungroup() |>
#   mutate(
#     estacion_label = if_else(row_number() == 1, "", str_to_sentence(estacion))
#   ) |>
#   mutate(fill = color_estaciones[estacion])

d_area_tsi <- tibble(
  estado = c(
    "Ultra-oligotrófico",
    "Oligotrófico",
    "Mesotrófico",
    "Eutrófico",
    "Hiper-eutrófico"
  ),
  ymin = c(0, 30, 40, 50, 70),
  ymax = c(30, 40, 50, 70, 100)
) |>
  mutate(ylabel = (ymax + ymin) / 2)

g_tsi <- ggplot(d_tsi, aes(fecha, tsi)) +
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
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_rect(
    data = d_area_tsi,
    aes(
      # xmin = min(d_tsi$fecha),
      # xmax = max(d_est$fecha),
      xmin = fecha_i,
      xmax = fecha_f,
      ymin = ymin,
      ymax = ymax,
      fill = estado
    ),
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    breaks = unique(d_area_tsi$estado),
    values = alpha(c("#BABBFE", "#AEFCFF", "#ABFEA2", "#FFE9B7", "#FFAFAE"), .4)
  ) +
  geom_hline(yintercept = seq(10, 100, 10), linetype = 1, linewidth = .2) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    color = "black",
    linewidth = .4,
    span = .3
  ) +
  geom_point(
    color = "black",
    size = 1,
    shape = 21,
    fill = "white",
    stroke = 1
  ) +
  # geom_vline(xintercept = d_est$fecha, linetype = 2, linewidth = .2) +
  geom_text(
    data = d_est,
    aes(fecha_label, I(1.01), label = estacion_label),
    inherit.aes = FALSE,
    size = tamaño_label_est,
    vjust = -.2,
    fontface = "bold"
  ) +
  geom_label(
    data = d_area_tsi,
    aes(I(0), ymax, label = estado),
    inherit.aes = FALSE,
    size = 2,
    hjust = 0,
    fill = "white",
    family = "Times New Roman",
    label.r = unit(0, "pt"),
    color = "black",
    linewidth = 0,
    vjust = 1
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 month"),
    labels = \(x) str_to_sentence(format(x, "%b %y")),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = scales::breaks_width(10),
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(0, 100)
  ) +
  coord_cartesian(clip = "off") +
  labs(y = "TSI", x = NULL) +
  theme_bw(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(10, 5, 5, 5),
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.ticks.x = element_line(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(14, "pt"),
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.clip = "off"
  )

guardar_png(
  plot = g_tsi,
  filename = "figura_tsi",
  ancho = 5,
  alto = 3
)
