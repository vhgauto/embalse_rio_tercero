# datos ------------------------------------------------------------------

algas_orden <- c("Diatomeas", "Clorofitas", "Cianobacterias", "Criptófitas")

d_micro <- read_csv("datos/base_de_datos_micro2.csv", show_col_types = FALSE) |>
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
    breaks = scales::breaks_width(1e5),
    labels = \(Q) {
      if_else(
        Q == 0,
        "0",
        scales::label_number(
          big.mark = ".",
          decimal.mark = ",",
          suffix = "E+6",
          scale = 1e-6
        )(Q)
      )
    }
  ) +
  scale_fill_manual(
    breaks = algas_orden,
    values = c("#F4A261", "#FFFF00", "#2A9D8F", "#B7DEE8")
  ) +
  labs(x = "Sitio de muestreo", y = "Abundancia (cel/L)") +
  guides(
    fill = guide_legend(
      override.aes = list(color = "black", linewidth = .1)
    )
  ) +
  theme_bw(base_size = 11, base_family = "Arial") +
  theme(
    plot.margin = margin(5, 8, 5, 5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.05), margin = margin(l = 1)),
    legend.key.spacing.x = unit(6, "mm"),
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
  plot = g_ciano,
  filename = "figura_ciano",
  ancho = 5,
  alto = 3
)

# browseURL("fig/2025-10/figura_ciano_2025_10.png")
