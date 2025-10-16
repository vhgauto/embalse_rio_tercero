library(showtext)
library(gt)
library(tidyverse)

font_add(
  family = "Times New Roman",
  regular = "fuentes/times.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

update_geom_defaults("text", aes(family = "Times New Roman"))

mes_actual <- 6
año_actual <- 2025

carpeta_fig <- paste0("fig/", año_actual, "-", mes_actual, "/")

if (!dir.exists(carpeta_fig)) {
  dir.create(carpeta_fig)
}

fecha_f <- ceiling_date(
  ymd(glue::glue("{año_actual}-{mes_actual}-01")),
  unit = "month"
) -
  days(1)

fecha_i <- floor_date(fecha_f - months(12), "month")

ancho_fig <- 5
alto_fig <- 3

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

color_estaciones <- c(
  Otoño = alpha("#FFD200", .4),
  Invierno = alpha("#00A8FF", .4),
  Primavera = alpha("#61BB25", .4),
  Verano = alpha("#FF0018", .4)
)

tamaño_label_est <- 2.5

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

d <- read_csv("datos/d.csv", show_col_types = FALSE)

promedio_tbl <- filter(d, param %in% c("temperatura", "ds", "cla")) |>
  reframe(
    m = mean(valor, na.rm = TRUE),
    .by = c(param, unidad)
  ) |>
  mutate(
    m_label = paste(
      format(
        round(m, 1),
        nsmall = 1,
        decimal.mark = ",",
        big.mark = ".",
        trim = TRUE
      ),
      unidad
    )
  )
