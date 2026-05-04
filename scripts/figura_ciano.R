# datos ------------------------------------------------------------------

algas_orden <- c("Diatomeas", "Clorofitas", "Cianobacterias", "Criptófitas")

d_micro <- read_csv("datos/base_de_datos_micro.csv", show_col_types = FALSE) |>
  mutate(algas = factor(algas, levels = algas_orden)) |>
  filter(
    month(fecha) == mes_actual & year(fecha) == año_actual
  )

escala_ciano_y <- 1e6

max_ciano_eje_y <- ceiling(max(d_micro$suma) / escala_ciano_y) * escala_ciano_y

# figura -----------------------------------------------------------------

g_ciano <- ggplot(d_micro, aes(sitio, suma, fill = algas)) +
  geom_col(position = position_stack()) +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(.01, .01), add = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05), add = c(0, 0)),
    breaks = scales::breaks_width(2e5),
    limits = c(0, max_ciano_eje_y),
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
        scales::label_number(scale = 1 / escala_ciano_y)(Q)
      )
    }
  ) +
  scale_fill_manual(
    breaks = algas_orden,
    values = c("#F4A261", "#FFFF00", "#2A9D8F", "#B7DEE8")
  ) +
  labs(x = "Sitio de muestreo", y = "Abundancia (millones de cel/L)") +
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
    legend.key.spacing.x = unit(5, "mm"),
    legend.key.height = unit(4, "mm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 30, 0, 0),
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

if (FALSE) {
  browseURL(paste0(
    "fig/",
    año_actual,
    "-",
    mes_actual_chr,
    "/figura_ciano_",
    año_actual,
    "_",
    mes_actual_chr,
    ".png"
  ))
}
