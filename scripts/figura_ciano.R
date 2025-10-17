# datos ------------------------------------------------------------------

algas_orden <- c("Diatomeas", "Clorofitas", "Cianobacterias", "Criptófitas")

d_micro <- read_csv("datos/d_micro.csv", show_col_types = FALSE) |>
  mutate(algas = factor(algas, levels = algas_orden))

# figura -----------------------------------------------------------------

g_ciano <- ggplot(d_micro, aes(sitio, suma, fill = algas)) +
  geom_col(position = position_stack()) +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(.01, .01), add = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05), add = c(0, 0)),
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      scale = 1e-3
    )
  ) +
  scale_fill_manual(
    breaks = algas_orden,
    values = c("#F4A261", "#FFFF00", "#2A9D8F", "#B7DEE8")
  ) +
  labs(y = "Abundancia, en miles (cel/ml)", x = "Sitios de muestreo") +
  theme_bw(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(margin = margin(r = 20, l = 5, t = 5, b = 5)),
    legend.key.height = unit(5, "pt"),
    legend.key.width = unit(15, "pt"),
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
  plot = g_ciano,
  filename = "figura_ciano",
  ancho = 5,
  alto = 3
)
