# paquetes ---------------------------------------------------------------

library(ggspatial)
library(tidyterra)

# mapa -------------------------------------------------------------------

# fecha_temp <- r_actual[!str_detect(r_actual, "temp")] |>
#   basename() |>
#   sub(".rds", "", x = _)

r_actual_temp <- "recortes/20251002_temp.tif"

ff_mascara <- function(S) {
  if_else(((S %/% 2^5) %% 2) == 1, 1, NA)
}

r_mask <- app(r$fmask, ff_mascara) |>
  mask(emb)
r_agua <- r * r_mask
r_temp <- rast(r_actual_temp)
r_temp_agua <- r_temp * r_mask

fecha_temp <- "20251002"

etq_temp <- paste0(
  "Producto HSL\nFecha: ",
  ymd(fecha_temp),
  "\nReflectancia de superficie"
)

mapa_temperatura <- ggplot() +
  geom_spatraster(data = r$nir, show.legend = FALSE, interpolate = FALSE) +
  scale_fill_gradient(low = "grey50", high = "grey90") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = r_temp_agua, interpolate = FALSE) +
  geom_segment(
    data = flechas_tbl,
    aes(x = xi, y = yi, xend = xf, yend = yf, group = id),
    arrow = arrow(angle = 10, length = unit(2, "mm"), type = "closed"),
    linewidth = .2
  ) +
  geom_spatvector(
    data = emb,
    fill = NA,
    color = "grey30",
    linewidth = .2
  ) +
  annotate(
    geom = "text",
    x = I(.99),
    y = I(.01),
    label = etq_temp,
    size = 3,
    family = "Times New Roman",
    hjust = 1,
    vjust = 0,
    lineheight = .8
  ) +
  scale_fill_gradientn(
    colors = RColorBrewer::brewer.pal(n = 11, name = "RdBu"),
    na.value = NA
  ) +
  annotation_north_arrow(
    location = "tl",
    style = north_arrow_fancy_orienteering(),
    height = unit(.8, "cm"),
    width = unit(.8, "cm"),
    pad_x = unit(0.25, "cm"),
    pad_y = unit(0.25, "cm")
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    width_hint = .1,
    text_family = "Times New Roman"
  ) +
  coord_sf(expand = FALSE) +
  labs(fill = "Temperatura (°C)") +
  theme_void(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.key.height = unit(7, "pt"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.text = element_text(size = 6, margin = margin(t = 2)),
    legend.title = element_text(margin = margin(t = 0, b = 10, r = 5))
  )

# guardo -----------------------------------------------------------------

guardar_png(
  plot = mapa_temperatura,
  filename = "mapa_temperatura",
  ancho = 5,
  alto = 5
)

# browseURL(paste0(getwd(), "/fig/2025-10/mapa_temperatura_2025_10.png"))
