# datos -------------------------------------------------------------------

algas_orden <- c("Diatomeas", "Clorofitas", "Cianobacterias", "Criptófitas")

d_micro <- read_csv("datos/base_de_datos_micro.csv", show_col_types = FALSE) |>
  mutate(algas = factor(algas, levels = algas_orden)) |>
  mutate(fecha = update(fecha, day = 1))

d_micro_suma <- reframe(d_micro, s = sum(suma), .by = fecha)

escala_ciano <- 1e6

max_ciano_y <- ceiling(max(d_micro_suma$s) / escala_ciano) * escala_ciano

# figura ------------------------------------------------------------------

g_ciano_mensual <- ggplot(d_micro, aes(fecha, suma, fill = algas)) +
  geom_col(position = position_stack()) +
  geom_line(
    data = d_micro_suma,
    aes(fecha, s),
    inherit.aes = FALSE,
    color = "black",
    linewidth = .5
  ) +
  geom_point(
    data = d_micro_suma,
    aes(fecha, s),
    inherit.aes = FALSE,
    size = 2,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = .7
  ) +
  scale_x_date(
    breaks = scales::breaks_width("1 month"),
    labels = \(x) str_to_sentence(format(x, "%b '%y"))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05), add = c(0, 0)),
    breaks = scales::breaks_width(2 * escala_ciano),
    limits = c(0, max_ciano_y),
    labels = \(Q) {
      if_else(
        Q == 0,
        "0",
        # scales::label_number(
        #   big.mark = ",",
        #   decimal.mark = ".",
        #   suffix = "E+6",
        #   scale = 1e-6
        # )(Q)
        scales::label_number(scale = 1 / escala_ciano)(Q)
      )
    }
  ) +
  scale_fill_manual(
    breaks = algas_orden,
    values = c("#F4A261", "#FFFF00", "#2A9D8F", "#B7DEE8")
  ) +
  labs(
    y = "Abundancia (millones de cel/L)",
    x = NULL,
    fill = NULL
  ) +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    plot.margin = margin(5, 8, 5, 5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.05), margin = margin(l = 1)),
    legend.key.spacing.x = unit(4, "mm"),
    legend.key.height = unit(4, "mm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 40, 0, 0),
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
  plot = g_ciano_mensual,
  filename = "figura_ciano_mensual",
  ancho = 5,
  alto = 3
)
