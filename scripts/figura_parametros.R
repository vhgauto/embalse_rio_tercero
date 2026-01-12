# variables --------------------------------------------------------------

parametros_interes <- c(
  "temperatura",
  "pH",
  "od",
  "ds",
  "sdt",
  "turb",
  "cla",
  "cla_ciano"
)

parametros_label <- c(
  "Temperatura del agua (°C)",
  "pH",
  "Oxígeno Disuelto (mg/L)",
  "Transparencia<br>del Agua (m)",
  "Sólidos disueltos totales (ppm)",
  "Turbidez (UNT)",
  "Concentración de<br>Clorofila-a (mg/m<sup>3</sup>)",
  "Clorofila-a de<br>Cianobacterias (mg/m<sup>3</sup>)"
)

parametros_tbl <- tibble(
  param = parametros_interes,
  parametros_label = parametros_label
) |>
  mutate(across(.cols = everything(), .fns = fct_inorder))

# datos ------------------------------------------------------------------

d_actual <- filter(
  d,
  month(fecha) == mes_actual & year(fecha) == año_actual
)
d_mes_anterior <- filter(
  d,
  month(fecha) == mes_anterior_X & year(fecha) == año_anterior_X
)
d_año_anterior <- filter(
  d,
  month(fecha) == mes_actual & year(fecha) == año_anterior_X
)

d_comp <- rbind(d_actual, d_mes_anterior, d_año_anterior) |>
  filter(param %in% parametros_interes) |>
  drop_na(valor) |>
  mutate(fecha_label = str_to_sentence(format(fecha, "%b '%y"))) |>
  mutate(fecha_label = fct_reorder(as.character(fecha_label), fecha)) |>
  inner_join(parametros_tbl, by = join_by(param)) |>
  reframe(
    param_m = mean(valor, na.rm = TRUE),
    param_sd = sd(valor, na.rm = TRUE),
    .by = c(parametros_label, fecha_label)
  )

# figura -----------------------------------------------------------------

col_param <- if (length(unique(d_comp$fecha_label)) == 3) {
  c("#9BBB58", "#4F81BC", "#990100")
} else {
  c("#4F81BC", "#990100")
}

g_param <- ggplot(d_comp, aes(fecha_label, param_m, fill = fecha_label)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = param_m - param_sd, ymax = param_m + param_sd),
    linewidth = .25,
    width = .2
  ) +
  facet_wrap(vars(parametros_label), nrow = 2, scales = "free") +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05), add = c(0, 0))
  ) +
  scale_fill_manual(
    values = col_param
  ) +
  labs(y = NULL, x = NULL) +
  theme_bw(base_size = 9, base_family = "Arial") +
  theme(
    plot.margin = margin(0, 5, 5, 5),
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
  ancho = 6.7,
  alto = 3
)
