# paquetes ---------------------------------------------------------------

library(terra)
library(ggspatial)
library(tidyterra)

# datos ------------------------------------------------------------------

mes_actual_chr <- if (mes_actual <= 9) paste0("0", mes_actual) else mes_actual

r_files <- list.files("recortes/", full.names = TRUE)
r_actual <- r_files[str_detect(r_files, paste0(año_actual, mes_actual_chr))]
r_actual <- r_actual[str_detect(r_actual, "rds")]

r <- readRDS(r_actual)
writeRaster(
  r,
  paste0(
    "recortes/",
    sub(".rds", "", basename(r_actual[!str_detect(r_actual, "temp")])),
    ".tif"
  ),
  overwrite = TRUE
)

r_actual_temp <- r_files[str_detect(
  r_files,
  paste0(año_actual, mes_actual_chr)
)]
r_actual_temp <- r_actual_temp[str_detect(r_actual_temp, "temp")]

r_temp <- rast(r_actual_temp)

ff_mascara <- function(S) {
  if_else(((S %/% 2^5) %% 2) == 1, 1, NA)
}

r_mask <- app(r$fmask, ff_mascara)
r_agua <- r * r_mask
r_temp_agua <- r_temp * r_mask

# máscara de agua --------------------------------------------------------

# mndwi <- (r$green - r$nir) / (r$green + r$nir)
# mndwi[is.infinite(mndwi)] <- NA

# agua <- thresh(mndwi, method = "mean")
# agua[isFALSE(agua)] <- NA

# r_temp_agua <- r_temp * agua

# mapa -------------------------------------------------------------------

fecha_temp <- r_actual[!str_detect(r_actual, "temp")] |>
  basename() |>
  sub(".rds", "", x = _)
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
  scale_fill_viridis_c(
    option = "turbo",
    na.value = NA,
    name = "Temperatura (°C)"
  ) +
  annotation_north_arrow(
    location = "tl",
    style = north_arrow_fancy_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm"),
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
  theme_void(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.key.height = unit(7, "pt"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
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
