# datos ------------------------------------------------------------------

# https://cordoba.redesclimaticas.com/next/reports?mss=30335

cota_vertedero <- 46.5
# cota_vertedero <- readxl::read_xlsx(
#   "datos/Base_ERT_OriginalActualizada.xlsx",
#   sheet = "3-Nivel_Diques-Cba"
# ) |>
#   select(fecha = 1, altura = "Embalse") |>
#   filter(fecha == "h labio de Vertedero") |>
#   pull(altura)

# d_cota_año <- readxl::read_xlsx(
#   "datos/Base_ERT_OriginalActualizada.xlsx",
#   sheet = "3-Nivel_Diques-Cba"
# ) |>
#   select(fecha = 1, altura = "Embalse") |>
#   filter(fecha != "h labio de Vertedero") |>
#   drop_na() |>
#   mutate(fecha = janitor::excel_numeric_to_date(as.numeric(fecha))) |>
#   filter(between(
#     fecha,
#     make_date(año_actual - 1, mes_actual, 1),
#     make_date(año_actual, mes_actual, 1)
#   ))

d_cota_año <- read_csv(
  "datos/base_de_datos_cotas.csv",
  show_col_types = FALSE
) |>
  filter(between(
    fecha,
    make_date(año_actual - 1, mes_actual, 1),
    make_date(año_actual, mes_actual, 1)
  ))

# figura -----------------------------------------------------------------

relleno <- grid::linearGradient(
  colours = hcl.colors(100, "Temps"),
  group = FALSE,
  x1 = 0,
  y1 = 0,
  x2 = 0,
  y2 = 1
)

min_cota_año <- floor(min(d_cota_año$altura)) - .5
max_cota_año <- ceiling(max(d_cota_año$altura)) + .5

d_fill <- tibble(
  ymin = seq(min_cota_año, max_cota_año, length.out = 200)
) |>
  mutate(ymax = lag(ymin)) |>
  drop_na() %>%
  mutate(fill = hcl.colors(nrow(.), "Temps"))

g_cota_año <- ggplot() +
  geom_rect(
    data = d_fill,
    aes(
      xmin = min(d_cota_año$fecha),
      xmax = max(d_cota_año$fecha),
      ymin = ymin,
      ymax = ymax,
      fill = fill
    ),
    linewidth = 0
  ) +
  geom_ribbon(
    data = d_cota_año,
    aes(fecha, ymin = altura, ymax = max_cota_año),
    color = "black",
    linewidth = .2,
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
    vjust = -.1,
    fill = alpha("white", .75),
    border.color = NA
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 month"),
    labels = \(x) str_to_sentence(format(x, "%b '%y")),
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(1),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_fill_identity() +
  coord_cartesian(ylim = c(min_cota_año, max_cota_año), expand = FALSE) +
  labs(x = NULL, y = "Nivel de presa (m)") +
  theme_bw(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(5, 10, 5, 5),
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
  plot = g_cota_año,
  filename = "figura_cota_año",
  ancho = 5,
  alto = 2
)
