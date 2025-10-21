# variables --------------------------------------------------------------

parametros_interes <- c("temperatura", "pH", "od", "ds", "cla", "cla_ciano")

parametros_label <- c(
  "Temperatura del agua (°C)",
  "pH",
  "Oxígeno Disuelto (mg/L)",
  "Transparencia del<br>Agua (m)",
  "Concentración de<br>Clorofila-a (mg/m<sup>3</sup>)",
  "Clorofila-a de<br>Cianobacterias (mg/m<sup>3</sup>)"
)

parametros_tbl <- tibble(
  param = parametros_interes,
  parametros_label = parametros_label
)

# datos ------------------------------------------------------------------

d_actual <- filter(
  d,
  month(fecha) == mes_actual & year(fecha) == año_actual
)
d_mes_anterior <- filter(
  d,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)
d_año_anterior <- filter(
  d,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

d_comp <- rbind(d_actual, d_mes_anterior, d_año_anterior) |>
  filter(param %in% parametros_interes) |>
  drop_na(valor) |>
  mutate(fecha_label = str_to_sentence(format(fecha, "%b '%y"))) |>
  mutate(fecha_label = fct_reorder(as.character(fecha_label), fecha)) |>
  inner_join(parametros_tbl, by = join_by(param)) |>
  mutate(param = factor(param, levels = parametros_interes)) |>
  mutate(parametros_label = fct_reorder(parametros_label, as.numeric(param)))

# figura -----------------------------------------------------------------

g_param <- ggplot(d_comp, aes(fecha_label, valor, fill = fecha_label)) +
  stat_summary(
    geom = "col",
    fun = median
  ) +
  stat_summary(
    geom = "errorbar",
    fun = "median",
    fun.min = \(x) median(x) - sd(x),
    fun.max = \(x) median(x) + sd(x),
    linewidth = .25,
    width = .2
  ) +
  facet_wrap(vars(parametros_label), scale = "free", nrow = 2) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05), add = c(0, 0)),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_fill_manual(
    values = c("#990100", "#9BBB58", "#4F81BC")
  ) +
  labs(y = NULL, x = NULL) +
  theme_bw(base_size = 9, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(10, 5, 5, 5),
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing.x = unit(14, "pt"),
    strip.text = ggtext::element_markdown(),
    strip.background = element_blank(),
    strip.clip = "off"
  )

# guardo -----------------------------------------------------------------

guardar_png(
  plot = g_param,
  filename = "figura_parametros",
  ancho = 5,
  alto = 3
)
