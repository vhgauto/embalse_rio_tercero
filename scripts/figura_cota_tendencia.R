# datos ------------------------------------------------------------------

# https://cordoba.redesclimaticas.com/next/reports?mss=30335

cota_vertedero <- 46.5

d_cota <- read_csv("datos/base_de_datos_cotas.csv", show_col_types = FALSE)

# figura -----------------------------------------------------------------

relleno <- grid::linearGradient(
  colours = hcl.colors(100, "Temps"),
  group = FALSE,
  x1 = 0,
  y1 = 0,
  x2 = 0,
  y2 = 1
)

min_cota <- floor(min(d_cota$altura)) - .5
max_cota <- ceiling(max(d_cota$altura)) + .5

d_fill <- tibble(
  ymin = seq(min_cota, max_cota, length.out = 200)
) |>
  mutate(ymax = lag(ymin)) |>
  drop_na() %>%
  mutate(fill = hcl.colors(nrow(.), "Temps"))

g_cota_hist <- ggplot() +
  geom_rect(
    data = d_fill,
    aes(
      xmin = min(d_cota$fecha),
      xmax = max(d_cota$fecha),
      ymin = ymin,
      ymax = ymax,
      fill = fill
    ),
    linewidth = 0
  ) +
  geom_ribbon(
    data = d_cota,
    aes(fecha, ymin = altura, ymax = max_cota),
    color = "black",
    linewidth = .1,
    fill = "white"
  ) +
  geom_hline(yintercept = cota_vertedero, linetype = 2) +
  annotate(
    geom = "label",
    x = I(0),
    y = cota_vertedero,
    label = paste0(
      "Cota de vertedero: ",
      format(cota_vertedero, big.mark = ".", decimal.mark = ","),
      " m"
    ),
    size = 3,
    hjust = 0,
    vjust = -.2,
    fill = alpha("white", .75),
    border.color = NA
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = scales::breaks_width(1),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_fill_identity() +
  coord_cartesian(ylim = c(min_cota, max_cota), expand = FALSE) +
  labs(x = NULL, y = "Nivel de presa (m)") +
  theme_bw(base_size = 8, base_family = "Arial") +
  theme(
    plot.margin = margin(10, 7, 5, 5),
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
  plot = g_cota_hist,
  filename = "figura_cota_historica",
  ancho = 5,
  alto = 2
)
