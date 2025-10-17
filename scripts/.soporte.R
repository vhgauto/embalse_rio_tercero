# paquetes ---------------------------------------------------------------

library(showtext)
library(tidyverse)

# funciones --------------------------------------------------------------

formato <- function(Z) {
  format(
    round(Z, 1),
    nsmall = 1,
    decimal.mark = ",",
    big.mark = ".",
    trim = TRUE
  )
}

guardar_png <- function(plot, filename, ancho, alto) {
  ggsave(
    plot = plot,
    filename = paste0(
      carpeta_fig,
      filename,
      "_",
      año_actual,
      "_",
      mes_actual,
      ".png"
    ),
    width = ancho,
    height = alto
  )
}

# fuentes ----------------------------------------------------------------

font_add(
  family = "Times New Roman",
  regular = "fuentes/times.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

update_geom_defaults("text", aes(family = "Times New Roman"))

# datos ------------------------------------------------------------------

d <- read_csv("datos/d.csv", show_col_types = FALSE) |>
  mutate(unidad = if_else(is.na(unidad), "", unidad))

promedio_tbl <- filter(
  d,
  param %in%
    c("temperatura", "ds", "cla", "pH", "od", "sdt", "turb", "cla_ciano")
) |>
  reframe(
    m = mean(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    .by = c(param, unidad)
  )

# mes y año --------------------------------------------------------------

mes_actual <- 6
año_actual <- 2025

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

tamaño_label_est <- 2.5

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

# pH ---------------------------------------------------------------------

ph0_tbl <- filter(d, param == "pH") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ph_m = mean(valor, na.rm = TRUE),
    ph_sd = sd(valor, na.rm = TRUE),
    ph_min = min(valor, na.rm = TRUE),
    ph_max = max(valor, na.rm = TRUE)
  )

ph0 <- formato(ph0_tbl$ph_m)
ph0_sd <- formato(ph0_tbl$ph_sd)
ph0_min <- formato(ph0_tbl$ph_min)
ph0_max <- formato(ph0_tbl$ph_max)

ph_p <- formato(filter(promedio_tbl, param == "pH")$m)
ph_p_sd <- formato(filter(promedio_tbl, param == "pH")$sd)

# oxígeno disuelto -------------------------------------------------------

od_tbl <- filter(d, param == "od") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    od_m = mean(valor, na.rm = TRUE),
    od_sd = sd(valor, na.rm = TRUE),
    od_min = min(valor, na.rm = TRUE),
    od_max = max(valor, na.rm = TRUE),
  )

od0 <- formato(od_tbl$od_m)
od0_sd <- formato(od_tbl$od_sd)
od0_min <- formato(od_tbl$od_min)
od0_max <- formato(od_tbl$od_max)

od_p <- formato(filter(promedio_tbl, param == "od")$m)
od_p_sd <- formato(filter(promedio_tbl, param == "od")$sd)

# transparencia ----------------------------------------------------------

ds_tbl <- filter(d, param == "ds") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ds_m = mean(valor, na.rm = TRUE),
    ds_sd = sd(valor, na.rm = TRUE),
    ds_min = min(valor, na.rm = TRUE),
    ds_max = max(valor, na.rm = TRUE)
  )

ds0 <- formato(ds_tbl$ds_m)
ds0_sd <- formato(ds_tbl$ds_sd)
ds0_min <- formato(ds_tbl$ds_min)
ds0_max <- formato(ds_tbl$ds_max)

ds_p <- formato(filter(promedio_tbl, param == "ds")$m)
ds_p_sd <- formato(filter(promedio_tbl, param == "ds")$sd)

# sólidos disueltos totales ----------------------------------------------

sdt_tbl <- filter(d, param == "sdt") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    sdt_m = mean(valor, na.rm = TRUE),
    sdt_sd = sd(valor, na.rm = TRUE),
    sdt_min = min(valor, na.rm = TRUE),
    sdt_max = max(valor, na.rm = TRUE)
  )

sdt0 <- formato(sdt_tbl$sdt_m)
sdt0_sd <- formato(sdt_tbl$sdt_sd)
sdt0_min <- formato(sdt_tbl$sdt_min)
sdt0_max <- formato(sdt_tbl$sdt_max)

sdt_p <- formato(filter(promedio_tbl, param == "sdt")$m)
sdt_p_sd <- formato(filter(promedio_tbl, param == "sdt")$sd)

# turbidez ---------------------------------------------------------------

turb_tbl <- filter(d, param == "turb") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    turb_m = mean(valor, na.rm = TRUE),
    turb_sd = sd(valor, na.rm = TRUE),
    turb_min = min(valor, na.rm = TRUE),
    turb_max = max(valor, na.rm = TRUE)
  )

turb0 <- formato(turb_tbl$turb_m)
turb0_sd <- formato(turb_tbl$turb_sd)
turb0_min <- formato(turb_tbl$turb_min)
turb0_max <- formato(turb_tbl$turb_max)

turb_p <- formato(filter(promedio_tbl, param == "turb")$m)
turb_p_sd <- formato(filter(promedio_tbl, param == "turb")$sd)

# conductividad ----------------------------------------------------------

ce_tbl <- filter(d, param == "ce") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    ce_m = mean(valor, na.rm = TRUE),
    ce_sd = sd(valor, na.rm = TRUE),
    ce_min = min(valor, na.rm = TRUE),
    ce_max = max(valor, na.rm = TRUE)
  )

ce0 <- formato(ce_tbl$ce_m)
ce0_sd <- formato(ce_tbl$ce_sd)
ce0_min <- formato(ce_tbl$ce_min)
ce0_max <- formato(ce_tbl$ce_max)

ce_p <- formato(filter(promedio_tbl, param == "ce")$m)
ce_p_sd <- formato(filter(promedio_tbl, param == "ce")$sd)

# clorofila-a ------------------------------------------------------------

cla_tbl <- filter(d, param == "cla") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    cla_m = mean(valor, na.rm = TRUE),
    cla_sd = sd(valor, na.rm = TRUE),
    cla_min = min(valor, na.rm = TRUE),
    cla_max = max(valor, na.rm = TRUE)
  )

cla0 <- formato(cla_tbl$cla_m)
cla0_sd <- formato(cla_tbl$cla_sd)
cla0_min <- formato(cla_tbl$cla_min)
cla0_max <- formato(cla_tbl$cla_max)

cla_p <- formato(filter(promedio_tbl, param == "cla")$m)
cla_p_sd <- formato(filter(promedio_tbl, param == "cla")$sd)

# cianobacterias ---------------------------------------------------------

cla_ciano_tbl <- filter(d, param == "cla_ciano") |>
  filter(between(fecha, fecha_i, fecha_f)) |>
  drop_na(valor) |>
  reframe(
    cla_ciano_m = mean(valor, na.rm = TRUE),
    cla_ciano_sd = sd(valor, na.rm = TRUE),
    cla_ciano_min = min(valor, na.rm = TRUE),
    cla_ciano_max = max(valor, na.rm = TRUE)
  )

cla_ciano0 <- formato(cla_ciano_tbl$cla_ciano_m)
cla_ciano0_sd <- formato(cla_ciano_tbl$cla_ciano_sd)
cla_ciano0_min <- formato(cla_ciano_tbl$cla_ciano_min)
cla_ciano0_max <- formato(cla_ciano_tbl$cla_ciano_max)

cla_ciano_p <- formato(filter(promedio_tbl, param == "cla_ciano")$m)
cla_ciano_p_sd <- formato(filter(promedio_tbl, param == "cla_ciano")$sd)
