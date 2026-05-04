# paquetes ---------------------------------------------------------------

library(rstac)
library(tidyverse)
library(terra)

source(file = "scripts/.soporte.R")

# datos ------------------------------------------------------------------

fecha_actual <- filter(
  d,
  month(fecha) == mes_actual & year(fecha) == año_actual
) |>
  distinct(fecha) |>
  pull(fecha)

earthdatalogin::edl_netrc()

s <- stac("https://cmr.earthdata.nasa.gov/stac/LPCLOUD/")

HLS_col <- list("HLSS30_2.0", "HLSL30_2.0")

roi <- terra::vect("vector/roi_embalse.geojson") |>
  project("EPSG:4326")
roi_extent <- terra::ext(roi)
bbox <- c(roi_extent$xmin, roi_extent$ymin, roi_extent$xmax, roi_extent$ymax)

roi_datetime <- paste0(
  fecha_actual - 2,
  "T00:00:00Z/",
  fecha_actual + 2,
  "T23:59:59Z"
)

items <- s |>
  stac_search(
    collections = HLS_col,
    bbox = bbox,
    datetime = roi_datetime,
    limit = 100
  ) |>
  post_request()

f_fecha_item <- function(E) {
  items$features[[E]]$properties$start_datetime |>
    str_sub(1, 10) |>
    ymd()
}

fechas_items <- map_vec(1:length(items$features), f_fecha_item)

fecha_i <- which.min(abs(fechas_items - fecha_actual))

fecha_hls <- fechas_items[fecha_i]
# fecha_hls <- fechas_items[1]

sf_items <- items_as_sf(items)
granule_id <- sapply(items$features, function(feature) feature$id)
fecha_feature <- map_vec(sf_items$datetime, \(Z) ymd(str_sub(Z, 1, 10)))
sf_items <- cbind(
  granule = granule_id,
  fecha_feature = fecha_feature,
  sf_items
)

extract_asset_urls <- function(feature) {
  collection_id <- feature$collection
  if (collection_id == "HLSS30_2.0") {
    bands = c("B01", "B02", "B03", "B04", "B8A", "B11", "B12", "Fmask")
  } else if (collection_id == "HLSL30_2.0") {
    bands = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "Fmask")
  }
  sapply(bands, function(band) feature$assets[[band]]$href)
}

asset_urls <- t(sapply(items$features, extract_asset_urls))

bandas_nombres <- c(
  "aerosol",
  "blue",
  "green",
  "red",
  "nir",
  "swir1",
  "swir2",
  "fmask"
)
colnames(asset_urls) <- bandas_nombres
sf_items <- cbind(sf_items, asset_urls) #|>
# filter(fecha_feature == fecha_hls)

# selección de LANDSAT en caso de haber dos productos para la misma fecha
if (nrow(sf_items) > 1) {
  sf_items <- dplyr::filter(sf_items, str_detect(granule, "L30"))
}

setGDALconfig("GDAL_HTTP_UNSAFESSL", value = "YES")
setGDALconfig("GDAL_HTTP_COOKIEFILE", value = ".rcookies")
setGDALconfig("GDAL_HTTP_COOKIEJAR", value = ".rcookies")
setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", value = "EMPTY_DIR")
setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", value = "TIF")

open_hls <- function(url, roi = NULL, nombre) {
  # Add VSICURL prefix
  url <- paste0('/vsicurl/', url)
  # Retrieve metadata
  meta <- describe(url)
  # Check if dataset is Quality Layer (Fmask) - no scaling this asset (int8 datatype)
  is_fmask <- any(grep("Fmask", meta))
  # Check if Scale is present in band metadata
  will_autoscale <- any(grep("Scale:", meta))
  # Read the raster
  r <- rast(url)
  names(r) <- nombre
  # Apply Scale Factor if necessary
  if (!will_autoscale && !is_fmask) {
    print(paste(
      "No scale factor found in band metadata.",
      "Applying scale factor of 0.0001 to",
      basename(url)
    ))
    r <- r * 0.0001
    names(r) <- nombre
  }
  # Crop if roi specified
  if (!is.null(roi)) {
    # Reproject roi to match crs of r
    roi_reproj <- project(roi, crs(r))
    r <- mask(crop(r, roi_reproj), roi_reproj)
    names(r) <- nombre
  }
  return(r)
}

f_descarga <- function(X) {
  links <- as_tibble(sf_items) |>
    select(any_of(X)) |>
    pull()

  map(links, ~ open_hls(.x, roi = roi, nombre = X))
}

rasters <- map(bandas_nombres, f_descarga)
names(rasters) <- bandas_nombres

f_mascara <- function(X) {
  X <- as.numeric(X)
  if_else(
    # ((X %/% 2^1) %% 2) != 1 & ((X %/% 2^3) %% 2) != 1, <---- QUITO NUBES
    ((X %/% 2^5) %% 2) == 1, # <----- CONSERVO ÚNICAMENTE AGUA
    1,
    NA
  )
}

r_mask <- map(
  seq_along(rasters$fmask),
  ~ terra::app(rasters$fmask[[.x]], f_mascara)
) |>
  pluck(1)

raster_hls <- map(
  rasters,
  ~ pluck(.x) |>
    pluck(1)
) |>
  rast()

raster_hls_masked <- raster_hls * r_mask

fecha_recorte <- filter(
  sf_items,
  str_detect(granule, str_sub(unique(varnames(raster_hls))[1], 1, 29))
)$datetime |>
  str_sub(1, 10) |>
  gsub(pattern = "-", replacement = "", x = _)

f_write_raster <- function(Y) {
  terra::saveRDS(
    Y,
    paste0("recortes/", fecha_recorte, ".rds"),
    # overwrite = TRUE
    # datatype = "INT8U"
  )
  print(ymd(fecha_recorte))
}

f_write_raster(raster_hls_masked)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ---------------------------------------

aerosol_stack <- lapply(
  sf_items$aerosol,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[1]
)
blue_stack <- lapply(
  sf_items$blue,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[2]
)
green_stack <- lapply(
  sf_items$green,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[3]
)
red_stack <- lapply(
  sf_items$red,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[4]
)
nir_stack <- lapply(
  sf_items$nir,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[5]
)
swir1_stack <- lapply(
  sf_items$swir1,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[6]
)
swir2_stack <- lapply(
  sf_items$swir2,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[7]
)
fmask_stack <- lapply(
  sf_items$fmask,
  open_hls,
  roi = roi,
  nombre = bandas_nombres[8]
)

selected_bit_nums <- c(1, 2, 3, 4)

build_mask <- function(fmask, selected_bit_nums) {
  # Create a mask of all zeros
  mask <- rast(fmask, vals = 0)
  for (b in selected_bit_nums) {
    # Apply Bitwise AND to fmask values and selected bit numbers
    mask_temp <- app(fmask, function(x) bitwAnd(x, bitwShiftL(1, b)) > 0)
    # Update Mask to maintain only 1 layer with bitwise OR
    mask <- mask | mask_temp
  }
  return(mask)
}

qmask_stack <- lapply(
  fmask_stack,
  build_mask,
  selected_bit_nums = selected_bit_nums
)

f_masked <- function(W) {
  mapply(
    function(x, y) {
      mask(x, y, maskvalue = TRUE, updatevalue = NA)
    },
    W,
    qmask_stack,
    SIMPLIFY = FALSE
  )
}

ff_mascara <- function(S) {
  S <- as.numeric(S)
  if_else(
    ((S %/% 2^1) %% 2) != 1 & ((S %/% 2^3) %% 2) != 1,
    1,
    NA
  )
}

ff_bit <- terra::app(fmask_stack[[1]], ff_mascara)

# aerosol_masked <- f_masked(aerosol_stack)
# blue_masked <- f_masked(blue_stack)
# green_masked <- f_masked(green_stack)
# red_masked <- f_masked(red_stack)
# nir_masked <- f_masked(nir_stack)
# swir1_masked <- f_masked(swir1_stack)
# swir2_masked <- f_masked(swir2_stack)
# fmask_masked <- f_masked(fmask_stack)

# aerosol_masked <- aerosol_stack[[1]] * ff_bit
# blue_masked <- blue_stack[[1]] * ff_bit
# green_masked <- green_stack[[1]] * ff_bit
# red_masked <- red_stack[[1]] * ff_bit
# nir_masked <- nir_stack[[1]] * ff_bit
# swir1_masked <- swir1_stack[[1]] * ff_bit
# swir2_masked <- swir2_stack[[1]] * ff_bit
# fmask_masked <- fmask_stack[[1]] * ff_bit

aerosol_masked <- aerosol_stack[[1]]
blue_masked <- blue_stack[[1]]
green_masked <- green_stack[[1]]
red_masked <- red_stack[[1]]
nir_masked <- nir_stack[[1]]
swir1_masked <- swir1_stack[[1]]
swir2_masked <- swir2_stack[[1]]
fmask_masked <- fmask_stack[[1]]

f_recorte <- function(Q) {
  r <- rast(
    list(
      aerosol_masked[[Q]],
      blue_masked[[Q]],
      green_masked[[Q]],
      red_masked[[Q]],
      nir_masked[[Q]],
      swir1_masked[[Q]],
      swir2_masked[[Q]],
      fmask_masked[[Q]]
    )
  )
}

lista_recortes <- f_recorte(1)

fecha_recorte <- filter(
  sf_items,
  str_detect(granule, str_sub(unique(varnames(lista_recortes))[1], 1, 29))
)$datetime |>
  str_sub(1, 10) |>
  gsub(pattern = "-", replacement = "", x = _)

f_write_raster <- function(Y) {
  terra::saveRDS(
    Y,
    paste0("recortes/", fecha_recorte, ".rds"),
    # overwrite = TRUE
    # datatype = "INT8U"
  )
  print(ymd(fecha_recorte))
}

f_write_raster(lista_recortes)

# temperatura ------------------------------------------------------------

earthdatalogin::edl_netrc()

s <- stac("https://cmr.earthdata.nasa.gov/stac/LPCLOUD/")

roi_datetime_temp <- paste0(
  max(d$fecha) - 3,
  "T00:00:00Z/",
  max(d$fecha) + 3,
  "T23:59:59Z"
)

roi <- terra::vect("vector/roi_embalse.geojson") |>
  project("EPSG:4326")
roi_extent <- terra::ext(roi)
bbox <- c(roi_extent$xmin, roi_extent$ymin, roi_extent$xmax, roi_extent$ymax)

items_temp <- s |>
  stac_search(
    collections = "HLSL30_2.0",
    bbox = bbox,
    datetime = roi_datetime_temp,
    limit = 100
  ) |>
  post_request()

sf_items_temp <- items_as_sf(items_temp)
granule_id_temp <- sapply(items_temp$features, function(feature) feature$id)
fecha_feature_temp <- map_vec(sf_items_temp$datetime, \(Z) {
  ymd(str_sub(Z, 1, 10))
})
sf_items_temp <- cbind(
  granule = granule_id_temp,
  fecha_feature = fecha_feature_temp,
  sf_items_temp
)

bandas_nombres_temp <- c(
  "B01",
  "B02",
  "B03",
  "B04",
  "B05",
  "B06",
  "B07",
  "B10",
  "B11",
  "Fmask"
)
bandas_nombres_temp_label <- c(
  "B01",
  "B02",
  "B03",
  "B04",
  "B05",
  "B06",
  "B07",
  "temp_10",
  "temp_11",
  "fmask"
)

extract_asset_urls_temp <- function(feature) {
  sapply(bandas_nombres_temp, function(band) {
    feature$assets[[band]]$href
  })
}

asset_urls_temp <- t(sapply(items_temp$features, extract_asset_urls_temp))
colnames(asset_urls_temp) <- bandas_nombres_temp_label
sf_items_temp <- cbind(sf_items_temp, asset_urls_temp)

open_hls_temp <- function(url, roi = NULL, nombre) {
  # Add VSICURL prefix
  url <- paste0('/vsicurl/', url)
  r <- rast(url)
  names(r) <- nombre
  if (!is.null(roi)) {
    # Reproject roi to match crs of r
    roi_reproj <- project(roi, crs(r))
    r <- mask(crop(r, roi_reproj), roi_reproj)
    names(r) <- nombre
  }
  return(r)
}


f_descarga_temp <- function(X) {
  links <- as_tibble(sf_items_temp) |>
    select(any_of(X)) |>
    pull()

  map(links, ~ open_hls_temp(.x, roi = roi, nombre = X))
}

rasters_temp <- map(bandas_nombres_temp_label, f_descarga_temp)
names(rasters_temp) <- bandas_nombres_temp_label

f_mascara_temp <- function(X) {
  X <- as.numeric(X)
  if_else(
    # ((X %/% 2^1) %% 2) != 1 & ((X %/% 2^3) %% 2) != 1, <---- QUITO NUBES
    ((X %/% 2^5) %% 2) == 1, # <----- CONSERVO ÚNICAMENTE AGUA
    1,
    NA
  )
}

r_mask_temp <- map(
  seq_along(rasters_temp$fmask),
  ~ terra::app(rasters_temp$fmask[[.x]], f_mascara_temp)
) |>
  pluck(1)

raster_hls_temp <- map(
  rasters_temp,
  ~ pluck(.x) |>
    pluck(1)
) |>
  rast()

raster_hls_masked_temp <- raster_hls_temp * r_mask_temp

fecha_recorte_temp <- filter(
  sf_items_temp,
  str_detect(granule, str_sub(unique(varnames(raster_hls_temp))[1], 1, 29))
)$datetime |>
  str_sub(1, 10) |>
  gsub(pattern = "-", replacement = "", x = _)

writeRaster(
  raster_hls_masked_temp,
  paste0("recortes/", fecha_recorte_temp, "_temp.tif"),
  overwrite = TRUE
)


temp_stack_b1 <- open_hls_temp(
  url = sf_items_temp$B01,
  roi = roi,
  nombre = bandas_l30_label[1]
)

temp_stack_b2 <- open_hls_temp(
  url = sf_items_temp$B02,
  roi = roi,
  nombre = bandas_l30_label[2]
)

temp_stack_b3 <- open_hls_temp(
  url = sf_items_temp$B03,
  roi = roi,
  nombre = bandas_l30_label[3]
)

temp_stack_b4 <- open_hls_temp(
  url = sf_items_temp$B04,
  roi = roi,
  nombre = bandas_l30_label[4]
)

temp_stack_b5 <- open_hls_temp(
  url = sf_items_temp$B05,
  roi = roi,
  nombre = bandas_l30_label[5]
)

temp_stack_b6 <- open_hls_temp(
  url = sf_items_temp$B06,
  roi = roi,
  nombre = bandas_l30_label[6]
)

temp_stack_b7 <- open_hls_temp(
  url = sf_items_temp$B07,
  roi = roi,
  nombre = bandas_l30_label[7]
)

temp_stack_b10 <- open_hls_temp(
  url = sf_items_temp$temp_10,
  roi = roi,
  nombre = bandas_l30_label[8]
)

temp_stack_b11 <- open_hls_temp(
  url = sf_items_temp$temp_11,
  roi = roi,
  nombre = bandas_l30_label[9]
)

temp_stack_fmask <- open_hls_temp(
  url = sf_items_temp$fmask,
  roi = roi,
  nombre = bandas_l30_label[10]
)

ff_bit_temp <- terra::app(temp_stack_fmask, ff_mascara)

r_temp_masked <- rast(
  list(
    temp_stack_b1 * ff_bit_temp,
    temp_stack_b2 * ff_bit_temp,
    temp_stack_b3 * ff_bit_temp,
    temp_stack_b4 * ff_bit_temp,
    temp_stack_b5 * ff_bit_temp,
    temp_stack_b6 * ff_bit_temp,
    temp_stack_b7 * ff_bit_temp,
    temp_stack_b10 * ff_bit_temp,
    temp_stack_b11 * ff_bit_temp,
    temp_stack_fmask * ff_bit_temp
  )
)

fecha_recorte_temp <- filter(
  sf_items_temp,
  str_detect(granule, str_sub(unique(varnames(r_temp_masked))[1], 1, 29))
)$datetime |>
  str_sub(1, 10) |>
  gsub(pattern = "-", replacement = "", x = _)

writeRaster(
  r_temp_masked,
  paste0("recortes/", fecha_recorte_temp, "_temp.tif"),
  overwrite = TRUE
)

# pp <- c(stack_b2, stack_b3, stack_b4, temp_stack_10)
# writeRaster(pp, "recortes/pp.tif")

# plotRGB(pp, r = 3, g = 2, b = 1, scale = .1, stretch = "lin")

# library(tidyterra)

# ggplot() +
#   geom_spatraster_rgb(
#     data = pp,
#     r = 3,
#     g = 2,
#     b = 1,
#     max_col_value = .2,
#     # stretch = "hist",
#     interpolate = FALSE
#   )
