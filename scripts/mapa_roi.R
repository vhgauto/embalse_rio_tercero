# paquetes ---------------------------------------------------------------

library(terra)
library(tidyterra)
library(ggspatial)
library(tidyverse)

# vectores ---------------------------------------------------------------

v <- ext(-64.543133, -64.398079, -32.271749, -32.155559) |>
  vect(crs = "EPSG:4326")
e <- ext(r)
w <- vect("vector/argentina-251016-free/gis_osm_water_a_free_1.shp")
w_crop <- crop(w, e)
embalse <- w_crop2[w_crop$name == "Embalse Ministro Juan Pistarini"]
writeVector(embalse, "vector/embalse.gpkg")
embalse <- vect("vector/embalse.gpkg")

ciudades <- tibble(
  ciudad = c(
    "San\nIgnacio",
    "Villa del\nDique",
    "Villa\nRumipal",
    "Embalse",
    "Central Nuclear\nEmbalse (CNE)",
    "Villa\nQuillinzo"
  ),
  lat = c(
    -32.165,
    -32.163,
    -32.189519,
    -32.20452,
    -32.232,
    -32.233056
  ),
  lon = c(
    -64.516944,
    -64.487,
    -64.479214,
    -64.40048,
    -64.443,
    -64.511944
  )
) |>
  vect(geom = c("lon", "lat"), crs = "EPSG:4326")

p <- dplyr::filter(d, fecha == max(d$fecha, na.rm = TRUE)) |>
  distinct(latitud, longitud) |>
  mutate(id = 1:11) |>
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326")

arg <- vect("vector/dptos_pcias_continental.gpkg") |>
  project("EPSG:4326")

pcia <- vect("vector/pcias_continental.gpkg") |>
  project("EPSG:4326")

cba <- arg[arg$provincia == "Córdoba"]
cba_e <- vect(ext(cba) * 1.1, "EPSG:4326")

pcia_crop <- crop(pcia, cba_e)

lobulos <- tibble(
  x = c(-64.47299, -64.43106),
  y = c(-32.21105, -32.19168),
  id = c("Lóbulo Oeste", "Lóbulo Este")
) |>
  vect(geom = c("x", "y"), crs = "EPSG:4326")

# ráster -----------------------------------------------------------------

# rgb <- maptiles::get_tiles(
#   v,
#   provider = "Esri.WorldImagery",
#   zoom = 15,
#   crop = TRUE
# )
# writeRaster(rgb, "recortes/rgb.tif", overwrite = TRUE)
r <- rast("recortes/rgb.tif")

# mapa -------------------------------------------------------------------

g_rgb <- ggplot() +
  geom_spatraster_rgb(
    data = r,
    maxcell = prod(dim(r)[c(1, 2)])
  ) +
  geom_spatvector(
    data = embalse,
    fill = "#409BE1",
    color = NA
  ) +
  geom_spatvector(
    data = ciudades,
    shape = 24,
    color = "black",
    fill = "tomato",
    size = 4
  ) +
  geom_spatvector(
    data = p,
    color = "black",
    fill = "gold",
    shape = 21,
    size = 6
  ) +
  geom_spatvector_text(
    data = p,
    aes(label = id),
    family = "Times New Roman",
    fontface = "bold",
    size = 4
  ) +
  shadowtext::geom_shadowtext(
    data = terra::as.data.frame(ciudades, geom = "xy"),
    aes(x, y - .0013, label = ciudad),
    lineheight = .8,
    vjust = 1,
    family = "Times New Roman",
  ) +
  shadowtext::geom_shadowtext(
    data = terra::as.data.frame(lobulos, geom = "xy"),
    aes(x, y, label = id),
    color = "gold",
    vjust = 1,
    family = "Times New Roman",
  ) +
  annotation_north_arrow(
    location = "tl",
    height = unit(1, "cm"),
    width = unit(.8, "cm"),
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm"),
    style = north_arrow_fancy_orienteering(
      text_col = "white",
      line_col = "white"
    )
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.6, "cm"),
    pad_y = unit(0.6, "cm"),
    text_family = "Times New Roman",
    line_col = "white",
    width_hint = .2
  ) +
  coord_sf(expand = FALSE) +
  theme_void()

ggsave(
  plot = g_rgb,
  filename = "fig/rgb.png",
  width = 20,
  height = 18.72,
  units = "cm"
)

# browseURL(paste0(getwd(), "/fig/rgb.png"))

g_cba <- ggplot() +
  geom_spatvector(
    data = pcia_crop,
    fill = "#CCD2D2",
    linewidth = 1
  ) +
  geom_spatvector(
    data = cba,
    fill = "#FFE79F",
    linewidth = .6
  ) +
  geom_spatvector(
    data = centroids(embalse),
    color = "#409BE1",
    size = 8
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_blank())

ggsave(
  plot = g_cba,
  filename = "fig/cba.png",
  width = 15,
  height = 24.3,
  units = "cm"
)

i_rgb <- magick::image_read("fig/rgb.png")
i_cba <- magick::image_read("fig/cba.png")

magick::image_append(i_rgb) |>
  magick::image_composite(
    composite_image = magick::image_scale(i_cba, "550x"),
    gravity = "southeast",
    offset = "+0+0"
  ) |>
  magick::image_write("fig/roi.png")
