# paquetes ---------------------------------------------------------------

library(showtext)
library(terra)
library(tidyverse)

# funciones --------------------------------------------------------------

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
      mes_actual,
      formato
    ),
    width = ancho,
    height = alto
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

    v <- nit_v[20:22] |>
      str_replace_all(",", ".") |>
      str_remove_all(" mg/L")
  }

  if (punto == 9) {
    nit_v <- tabulapdf::extract_text(nit_l[str_detect(
      nit_l,
      as.character(punto)
    )]) |>
      str_split("\n") |>
      pluck(1) |>
      str_replace_all("\r", "")

    v <- nit_v[18:20] |>
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

f_fig <- function(MES = mes_actual, AÑO = año_actual, PARAM, FORMATO = ".png") {
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

# fuentes ----------------------------------------------------------------

font_add(
  family = "Times New Roman",
  regular = "fuentes/times.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

# update_geom_defaults("text", aes(family = "Arial"))
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
  if_else(((S %/% 2^5) %% 2) == 1, 1, NA)
}

r_mask <- app(r$fmask, ff_mascara) |>
  mask(emb)
r_agua <- r * r_mask
r_temp <- rast(r_actual_temp)
r_temp_agua <- r_temp * r_mask

# mes y año --------------------------------------------------------------

# mes_actual <- 6
# año_actual <- 2025

# figuras ----------------------------------------------------------------

carpeta_fig <- paste0("fig/", año_actual, "-", mes_actual, "/")

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

# pH ---------------------------------------------------------------------

d_ph <- filter(d, param == "pH") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ph_m = median(valor, na.rm = TRUE),
    ph_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

ph0_tbl <- filter(
  d_ph,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

ph1_tbl <- filter(
  d_ph,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

ph2_tbl <- filter(
  d_ph,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

ph0 <- formato(ph0_tbl$ph_m)
ph0_sd <- formato(ph0_tbl$ph_sd)

ph1 <- formato(ph1_tbl$ph_m)
ph1_sd <- formato(ph1_tbl$ph_sd)

ph2 <- formato(ph2_tbl$ph_m)
ph2_sd <- formato(ph2_tbl$ph_sd)

ph_p <- formato(filter(promedio_tbl, param == "pH")$m)
ph_p_sd <- formato(filter(promedio_tbl, param == "pH")$sd)

# oxígeno disuelto -------------------------------------------------------

d_od <- filter(d, param == "od") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    od_m = median(valor, na.rm = TRUE),
    od_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

od0_tbl <- filter(
  d_od,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

od1_tbl <- filter(
  d_od,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

od2_tbl <- filter(
  d_od,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

od0 <- formato(od0_tbl$od_m)
od0_sd <- formato(od0_tbl$od_sd)

od1 <- formato(od1_tbl$od_m)
od1_sd <- formato(od1_tbl$od_sd)

od2 <- formato(od2_tbl$od_m)
od2_sd <- formato(od2_tbl$od_sd)

od_p <- formato(filter(promedio_tbl, param == "od")$m)
od_p_sd <- formato(filter(promedio_tbl, param == "od")$sd)

# transparencia ----------------------------------------------------------

d_ds <- filter(d, param == "ds") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ds_m = median(valor, na.rm = TRUE),
    ds_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

ds0_tbl <- filter(
  d_ds,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

ds1_tbl <- filter(
  d_ds,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

ds2_tbl <- filter(
  d_ds,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

ds0 <- formato(ds0_tbl$ds_m)
ds0_sd <- formato(ds0_tbl$ds_sd)

ds1 <- formato(ds1_tbl$ds_m)
ds1_sd <- formato(ds1_tbl$ds_sd)

ds2 <- formato(ds2_tbl$ds_m)
ds2_sd <- formato(ds2_tbl$ds_sd)

ds_p <- formato(filter(promedio_tbl, param == "ds")$m)
ds_p_sd <- formato(filter(promedio_tbl, param == "ds")$sd)

# sólidos disueltos totales ----------------------------------------------

d_sdt <- filter(d, param == "sdt") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    sdt_m = median(valor, na.rm = TRUE),
    sdt_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

sdt0_tbl <- filter(
  d_sdt,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

sdt1_tbl <- filter(
  d_sdt,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

sdt2_tbl <- filter(
  d_sdt,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

sdt0 <- formato(sdt0_tbl$sdt_m)
sdt0_sd <- formato(sdt0_tbl$sdt_sd)

sdt1 <- formato(sdt1_tbl$sdt_m)
sdt1_sd <- formato(sdt1_tbl$sdt_sd)

sdt2 <- formato(sdt2_tbl$sdt_m)
sdt2_sd <- formato(sdt2_tbl$sdt_sd)

sdt_p <- formato(filter(promedio_tbl, param == "sdt")$m)
sdt_p_sd <- formato(filter(promedio_tbl, param == "sdt")$sd)

# turbidez ---------------------------------------------------------------

d_turb <- filter(d, param == "turb") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    turb_m = median(valor, na.rm = TRUE),
    turb_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

turb0_tbl <- filter(
  d_turb,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

turb1_tbl <- filter(
  d_turb,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

turb2_tbl <- filter(
  d_turb,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

turb0 <- formato(turb0_tbl$turb_m)
turb0_sd <- formato(turb0_tbl$turb_sd)

turb1 <- formato(turb1_tbl$turb_m)
turb1_sd <- formato(turb1_tbl$turb_sd)

turb2 <- formato(turb2_tbl$turb_m)
turb2_sd <- formato(turb2_tbl$turb_sd)

turb_p <- formato(filter(promedio_tbl, param == "turb")$m)
turb_p_sd <- formato(filter(promedio_tbl, param == "turb")$sd)

# conductividad ----------------------------------------------------------

d_ce <- filter(d, param == "ce") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ce_m = median(valor, na.rm = TRUE),
    ce_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

ce0_tbl <- filter(
  d_ce,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

ce1_tbl <- filter(
  d_ce,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

ce2_tbl <- filter(
  d_ce,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

ce0 <- formato(ce0_tbl$ce_m)
ce0_sd <- formato(ce0_tbl$ce_sd)

ce1 <- formato(ce1_tbl$ce_m)
ce1_sd <- formato(ce1_tbl$ce_sd)

ce2 <- formato(ce2_tbl$ce_m)
ce2_sd <- formato(ce2_tbl$ce_sd)

ce_p <- formato(filter(promedio_tbl, param == "ce")$m)
ce_p_sd <- formato(filter(promedio_tbl, param == "ce")$sd)

# clorofila-a ------------------------------------------------------------

d_cla <- filter(d, param == "cla") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    cla_m = median(valor, na.rm = TRUE),
    cla_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

cla0_tbl <- filter(
  d_cla,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

cla1_tbl <- filter(
  d_cla,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

cla2_tbl <- filter(
  d_cla,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

cla0 <- formato(cla0_tbl$cla_m)
cla0_sd <- formato(cla0_tbl$cla_sd)

cla1 <- formato(cla1_tbl$cla_m)
cla1_sd <- formato(cla1_tbl$cla_sd)

cla2 <- formato(cla2_tbl$cla_m)
cla2_sd <- formato(cla2_tbl$cla_sd)

cla_p <- formato(filter(promedio_tbl, param == "cla")$m)
cla_p_sd <- formato(filter(promedio_tbl, param == "cla")$sd)

# cianobacterias ---------------------------------------------------------

d_ciano <- filter(d, param == "cla_ciano") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ciano_m = median(valor, na.rm = TRUE),
    ciano_sd = sd(valor, na.rm = TRUE),
    .by = fecha
  )

ciano0_tbl <- filter(
  d_ciano,
  month(fecha) == mes_actual & year(fecha) == año_actual
)

ciano1_tbl <- filter(
  d_ciano,
  month(fecha) == mes_actual - 1 & year(fecha) == año_actual
)

ciano2_tbl <- filter(
  d_ciano,
  month(fecha) == mes_actual & year(fecha) == año_actual - 1
)

ciano0 <- formato(ciano0_tbl$ciano_m)
ciano0_sd <- formato(ciano0_tbl$ciano_sd)

ciano1 <- formato(ciano1_tbl$ciano_m)
ciano1_sd <- formato(ciano1_tbl$ciano_sd)

ciano2 <- formato(ciano2_tbl$ciano_m)
ciano2_sd <- formato(ciano2_tbl$ciano_sd)

ciano_p <- formato(filter(promedio_tbl, param == "cla_ciano")$m)
ciano_p_sd <- formato(filter(promedio_tbl, param == "cla_ciano")$sd)
