library(terra)
library(tidyterra)

r <- rast("recortes/20250620.tif")
r_temp <- rast("recortes/20250620_temp.tif")

mndwi <- (r$green - r$nir) / (r$green + r$nir)
mndwi[is.infinite(mndwi)] <- NA

agua <- thresh(mndwi, method = "mean")
agua[isFALSE(agua)] <- NA

r_temp_agua <- r_temp * agua

mapa_temperatura <- ggplot() +
  geom_spatraster(data = r$nir, show.legend = FALSE) +
  scale_fill_gradient(low = "grey50", high = "grey90") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = r_temp_agua) +
  scale_fill_viridis_c(
    option = "turbo",
    na.value = NA,
    name = "Temperatura (°C)"
  ) +
  theme_void(base_size = 8, base_family = "Times New Roman") +
  theme(
    legend.position = "bottom",
    plot.margin = margin(b = 20),
    legend.key.height = unit(5, "pt")
  )

guardar_png(
  plot = mapa_temperatura,
  filename = "mapa_temperatura",
  ancho = 5,
  alto = 5
)
