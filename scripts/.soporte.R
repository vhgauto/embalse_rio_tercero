# paquetes ---------------------------------------------------------------

library(showtext)
library(terra)
library(tidyverse)

# funciones --------------------------------------------------------------

mes_anterior_X <- if (mes_actual == 1) 12 else mes_actual - 1
# año_anterior_X <- if (mes_actual == 1) año_actual - 1 else año_actual
año_anterior_X <- año_actual - 1
año_actual_X <- if (mes_actual == 1) año_actual - 1 else año_actual

formato <- function(Z, n = 1) {
  format(
    round(Z, n),
    nsmall = n,
    decimal.mark = ".",
    big.mark = "",
    trim = TRUE
  )
}

guardar_png <- function(plot, filename, ancho, alto, formato = ".png") {
  ggsave(
    plot = plot,
    filename = paste0(
      carpeta_fig,
      filename,
      "_",
      año_actual,
      "_",
      mes_actual_chr,
      formato
    ),
    width = ancho,
    height = alto,
    units = "in"
  )
}

f_pdf1 <- function(archivo) {
  tabulapdf::extract_tables(archivo, pages = 2, method = "stream") |>
    pluck(1) |>
    select(
      "parametro" = 1
    ) |>
    drop_na() |>
    filter(str_detect(parametro, ":")) |>
    separate_wider_delim(
      cols = parametro,
      delim = ":",
      names = c("param", "valor")
    ) |>
    mutate(valor = str_trim(valor)) |>
    filter(param != "Sólidos Disueltos Totales") |>

    mutate(unidad = str_replace(valor, ".+ ((\\w)+)", "\\1")) |>
    mutate(
      unidad = if_else(unidad %in% c("Incoloro", "Inodoro"), "-", unidad)
    ) |>
    mutate(unidad = if_else(param %in% c("RAS", "pH"), "-", unidad)) |>
    mutate(valor_n = str_remove(valor, unidad)) |>
    mutate(valor_n = if_else(is.na(valor_n), valor, valor_n)) |>
    select(param, valor_n, unidad) |>
    mutate(param = str_replace(param, "oC", "°C"))
}

f_pdf2 <- function(punto) {
  if (punto == 11) {
    nit_v <- tabulapdf::extract_text(nit_l[str_detect(
      nit_l,
      as.character(punto)
    )]) |>
      str_split("\n") |>
      pluck(1) |>
      str_replace_all("\r", "")

    v <- nit_v[17:18] |>
      str_replace_all(",", ".") |>
      str_remove_all(" mg/L")
  }

  if (punto == 9) {
    nit_v <- tabulapdf::extract_text(nit_l[str_detect(
      nit_l,
      paste0("Sitio ", as.character(punto))
    )]) |>
      str_split("\n") |>
      pluck(1) |>
      str_replace_all("\r", "")

    v <- nit_v[15:16] |>
      str_replace_all(",", ".") |>
      str_remove_all(" mg/L")
  }

  return(v)
}

f_ratio <- function(df, var1, var2) {
  d <- df |>
    select(x = any_of(var1), y = any_of(var2)) |>
    transmute(z = x / y)
  names(d) <- paste0(nombres_ds[var1], "_", nombres_ds[var2])
  return(d)
}

f_fig <- function(
  MES = mes_actual_chr,
  AÑO = año_actual,
  PARAM,
  FORMATO = ".png"
) {
  cat(
    "![](",
    "fig/",
    AÑO,
    "-",
    MES,
    "/",
    PARAM,
    "_",
    AÑO,
    "_",
    MES,
    FORMATO,
    ")",
    sep = ""
  )
}

f_promedio_actual <- function(parametro, estad) {
  dplyr::filter(
    d,
    param == parametro &
      (month(fecha) == mes_actual & year(fecha) == año_actual)
  ) |>
    reframe(
      m = mean(valor),
      sd = sd(valor)
    ) |>
    mutate(across(.cols = everything(), .fns = formato)) |>
    select(all_of(estad)) |>
    pull()
}

f_promedio_mes_anterior <- function(parametro, estad) {
  v <- filter(
    d,
    param == parametro &
      (month(fecha) == mes_anterior_X & year(fecha) == año_actual_X)
  ) |>
    reframe(
      m = mean(valor),
      sd = sd(valor)
    ) |>
    mutate(across(.cols = everything(), .fns = formato)) |>
    select(all_of(estad)) |>
    pull()

  if (v == "NA" | v == "NaN") {
    return("No se hizo medición")
  } else {
    v
  }
}

f_promedio_año_anterior <- function(parametro, estad) {
  v <- filter(
    d,
    param == parametro &
      (month(fecha) == mes_actual & year(fecha) == año_anterior_X)
  ) |>
    reframe(
      m = mean(valor),
      sd = sd(valor)
    ) |>
    mutate(across(.cols = everything(), .fns = formato)) |>
    select(all_of(estad)) |>
    pull()

  if (v == "NA" | v == "NaN") {
    return("No se hizo medición")
  } else {
    v
  }
}

# fuentes ----------------------------------------------------------------

font_add(
  family = "Times New Roman",
  regular = "fuentes/times.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

update_theme("text", aes(family = "Arial"))

# datos ------------------------------------------------------------------

source("scripts/lectura_excel.R")

d <- read_csv("datos/base_de_datos.csv", show_col_types = FALSE) |>
  mutate(unidad = if_else(is.na(unidad), "", unidad))

promedio_tbl <- filter(
  d,
  param %in%
    c("temperatura", "ds", "cla", "pH", "od", "sdt", "turb", "cla_ciano", "ce")
) |>
  reframe(
    m = mean(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    .by = c(param, unidad)
  )

# vectores ---------------------------------------------------------------

emb <- vect("vector/embalse.gpkg") |>
  project("EPSG:32620")

v <- filter(d, fecha == max(d$fecha) & param == "cla_ciano") |>
  select(punto, latitud, longitud, valor) |>
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326") |>
  project("EPSG:32620")

# rásters ----------------------------------------------------------------

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

ff_mascara <- function(S) {
  S <- as.numeric(S)
  if_else(((S %/% 2^5) %% 2) == 1, 1, NA)
}

r_mask <- app(r$fmask, ff_mascara) |>
  mask(emb)
r_agua <- r * r_mask
r_temp <- rast(r_actual_temp)
r_temp_agua <- r_temp * r_mask

# figuras ----------------------------------------------------------------

carpeta_fig <- paste0("fig/", año_actual, "-", mes_actual_chr, "/")

if (!dir.exists(carpeta_fig)) {
  dir.create(carpeta_fig)
}

ancho_fig <- 5
alto_fig <- 3

# fechas -----------------------------------------------------------------

fecha_f <- ceiling_date(
  ymd(glue::glue("{año_actual}-{mes_actual}-01")),
  unit = "month"
) -
  days(1)

fecha_i <- floor_date(fecha_f - months(12), "month")

# estilo -----------------------------------------------------------------

color_estaciones <- c(
  Otoño = alpha("#FFD200", .4),
  Invierno = alpha("#00A8FF", .4),
  Primavera = alpha("#61BB25", .4),
  Verano = alpha("#FF0018", .4)
)

tamaño_label_est <- 3.5

altura_estacion_label <- 1.09

tamaño_texto_legend <- 1.01

# estaciones -------------------------------------------------------------

est_tbl <- expand_grid(
  mes = c(3, 6, 9, 12),
  año = c(año_actual, año_actual - 1)
) |>
  mutate(fecha = make_date(year = año, month = mes, day = 21)) |>
  arrange(fecha)

d_est <- tibble(
  fecha = sort(
    c(
      unique(est_tbl$fecha),
      fecha_i,
      fecha_f
    )
  )
) |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  mutate(mes = month(fecha)) |>
  mutate(año = year(fecha)) |>
  mutate(dia = day(fecha)) |>
  mutate(fecha_x = make_date(2020, mes, dia)) |>
  mutate(
    estacion = case_when(
      between(fecha_x, ymd(20191221), ymd(20200320)) ~ "Verano",
      between(fecha_x, ymd(20200321), ymd(20200620)) ~ "Otoño",
      between(fecha_x, ymd(20200621), ymd(20200920)) ~ "Invierno",
      between(fecha_x, ymd(20200921), ymd(20201220)) ~ "Primavera",
      between(fecha_x, ymd(20201221), ymd(20210220)) ~ "Verano",
    )
  ) |>
  mutate(
    fecha_min = fecha,
    fecha_max = lead(fecha)
  ) |>
  drop_na() |>
  mutate(
    fecha_dif = fecha_max - fecha_min
  ) |>
  mutate(
    fecha_label = fecha_min + (fecha_max - fecha_min) / 2
  ) |>
  mutate(
    estacion_label = if_else(fecha_dif < 60, "", estacion)
  ) |>
  mutate(fill = color_estaciones[estacion])

# flechas ----------------------------------------------------------------

fecha_l <- 500

flechas_tbl <- tibble(
  xi = c(354407.1, 356248.9, 357523.3, 366323.8, 359643.2, 359005.6, 357553.3),
  yi = c(
    -3565935.7,
    -3563637.0,
    -3558520.2,
    -3561088.2,
    -3568365.8,
    -3571714.6,
    -3570229.4
  )
) |>
  mutate(ang = c(-45, -75, -65, 30, -135, 95, -30)) |>
  mutate(rad = ang * pi / 180) |>
  mutate(xf = xi + fecha_l * cos(rad)) |>
  mutate(yf = yi + fecha_l * sin(rad)) |>
  mutate(id = c(1))

flechas_x <- flechas_tbl |>
  select(starts_with("x"), id) |>
  pivot_longer(
    cols = -id,
    names_to = "coord_x",
    values_to = "x"
  ) |>
  select(id, x)

flechas_y <- flechas_tbl |>
  select(starts_with("y"), id) |>
  pivot_longer(
    cols = -id,
    names_to = "coord_y",
    values_to = "y"
  ) |>
  select(id, y)

# temperatura ------------------------------------------------------------

temp0 <- f_promedio_actual("temperatura", "m")
temp0_sd <- f_promedio_actual("temperatura", "sd")

temp1 <- f_promedio_mes_anterior("temperatura", "m")
temp1_sd <- f_promedio_mes_anterior("temperatura", "sd")

temp2 <- f_promedio_año_anterior("temperatura", "m")
temp2_sd <- f_promedio_año_anterior("temperatura", "sd")

temp_p <- formato(filter(promedio_tbl, param == "temperatura")$m)
temp_p_sd <- formato(filter(promedio_tbl, param == "temperatura")$sd)

# pH ---------------------------------------------------------------------

ph0 <- f_promedio_actual("pH", "m")
ph0_sd <- f_promedio_actual("pH", "sd")

ph1 <- f_promedio_mes_anterior("pH", "m")
ph1_sd <- f_promedio_mes_anterior("pH", "sd")

ph2 <- f_promedio_año_anterior("pH", "m")
ph2_sd <- f_promedio_año_anterior("pH", "sd")

# oxígeno disuelto -------------------------------------------------------

od0 <- f_promedio_actual("od", "m")
od0_sd <- f_promedio_actual("od", "sd")

od1 <- f_promedio_mes_anterior("od", "m")
od1_sd <- f_promedio_mes_anterior("od", "sd")

od2 <- f_promedio_año_anterior("od", "m")
od2_sd <- f_promedio_año_anterior("od", "sd")

od_p <- formato(filter(promedio_tbl, param == "od")$m)
od_p_sd <- formato(filter(promedio_tbl, param == "od")$sd)

# transparencia ----------------------------------------------------------

ds0 <- f_promedio_actual("ds", "m")
ds0_sd <- f_promedio_actual("ds", "sd")

ds1 <- f_promedio_mes_anterior("ds", "m")
ds1_sd <- f_promedio_mes_anterior("ds", "sd")

ds2 <- f_promedio_año_anterior("ds", "m")
ds2_sd <- f_promedio_año_anterior("ds", "sd")

ds_p <- formato(filter(promedio_tbl, param == "ds")$m)
ds_p_sd <- formato(filter(promedio_tbl, param == "ds")$sd)

# sólidos disueltos totales ----------------------------------------------

sdt0 <- f_promedio_actual("sdt", "m")
sdt0_sd <- f_promedio_actual("sdt", "sd")

sdt1 <- f_promedio_mes_anterior("sdt", "m")
sdt1_sd <- f_promedio_mes_anterior("sdt", "sd")

sdt2 <- f_promedio_año_anterior("sdt", "m")
sdt2_sd <- f_promedio_año_anterior("sdt", "sd")

sdt_p <- formato(filter(promedio_tbl, param == "sdt")$m)
sdt_p_sd <- formato(filter(promedio_tbl, param == "sdt")$sd)

# turbidez ---------------------------------------------------------------

turb0 <- f_promedio_actual("turb", "m")
turb0_sd <- f_promedio_actual("turb", "sd")

turb1 <- f_promedio_mes_anterior("turb", "m")
turb1_sd <- f_promedio_mes_anterior("turb", "sd")

turb2 <- f_promedio_año_anterior("turb", "m")
turb2_sd <- f_promedio_año_anterior("turb", "sd")

turb_p <- formato(filter(promedio_tbl, param == "turb")$m)
turb_p_sd <- formato(filter(promedio_tbl, param == "turb")$sd)

# conductividad ----------------------------------------------------------

ce0 <- f_promedio_actual("ce", "m")
ce0_sd <- f_promedio_actual("ce", "sd")

ce1 <- f_promedio_mes_anterior("ce", "m")
ce1_sd <- f_promedio_mes_anterior("ce", "sd")

ce2 <- f_promedio_año_anterior("ce", "m")
ce2_sd <- f_promedio_año_anterior("ce", "sd")

ce_p <- formato(filter(promedio_tbl, param == "ce")$m)
ce_p_sd <- formato(filter(promedio_tbl, param == "ce")$sd)

# clorofila-a ------------------------------------------------------------

cla0 <- f_promedio_actual("cla", "m")
cla0_sd <- f_promedio_actual("cla", "sd")

cla1 <- f_promedio_mes_anterior("cla", "m")
cla1_sd <- f_promedio_mes_anterior("cla", "sd")

cla2 <- f_promedio_año_anterior("cla", "m")
cla2_sd <- f_promedio_año_anterior("cla", "sd")

cla_p <- formato(filter(promedio_tbl, param == "cla")$m)
cla_p_sd <- formato(filter(promedio_tbl, param == "cla")$sd)

# cianobacterias ---------------------------------------------------------

ciano0 <- f_promedio_actual("cla_ciano", "m")
ciano0_sd <- f_promedio_actual("cla_ciano", "sd")

ciano1 <- f_promedio_mes_anterior("cla_ciano", "m")
ciano1_sd <- f_promedio_mes_anterior("cla_ciano", "sd")

ciano2 <- f_promedio_año_anterior("cla_ciano", "m")
ciano2_sd <- f_promedio_año_anterior("cla_ciano", "sd")

ciano_p <- formato(filter(promedio_tbl, param == "cla_ciano")$m)
ciano_p_sd <- formato(filter(promedio_tbl, param == "cla_ciano")$sd)
