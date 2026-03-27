library(tidyverse)

d1 <- read_csv("datos/base_de_datos.csv", show_col_types = FALSE) |>
  mutate(unidad = if_else(is.na(unidad), "", unidad))

parametros_nombre <- c(
  "temperatura",
  "pH",
  "od",
  "ds",
  "sdt",
  "turb",
  "ce",
  "cla",
  "cla_ciano"
)

parametros_unidad <- c(
  "°C",
  "",
  "mg/L",
  "m",
  "ppm",
  "UNT",
  "µS/cm",
  "mg/m<sup>3</sup>",
  "mg/m<sup>3</sup>"
)

parametros_etq <- c(
  "Temp.",
  "pH",
  "OD",
  "DS",
  "SDT",
  "Turb.",
  "CE",
  "Cl-a",
  "Cl-a Ciano."
)

parametros_tbl <- tibble(
  param = parametros_nombre,
  unidad = parametros_unidad,
  param_etq = parametros_etq
)

sitio_nombre <- c(
  "club Almafuerte",
  "S,5 = HOTELES",
  "Garganta",
  "Entrada Rios y CNE",
  "S,2 = CANAL ENFRIAMIENTO",
  "S,1 = CONFLUENCIA RIOS",
  "entrada rio Grande",
  "rio Santa Rosa",
  "S,4 = CENTRO",
  "S,6 = VILLA DEL DIQUE",
  "S,7 = MURALLON"
)

sitio_etq <- c(
  "Club Almafuerte",
  "Hoteles",
  "Garganta",
  "Entrada ríos y CNE",
  "Canal de enfriamiento",
  "Confluencia ríos",
  "Ingreso río Grande",
  "Ingreso río Santa Rosa",
  "Centro",
  "Villa del dique",
  "Prensa"
)

sitio_tbl <- tibble(sitio = sitio_nombre, sitio_etq = sitio_etq)

mes_d <- month(max(d1$fecha))
año_d <- year(max(d1$fecha))

if (mes_d == mes_actual & año_d == año_actual) {
  TRUE
} else {
  # google sheet
  readxl::read_xlsx(
    path = "datos/Base_ERT_OriginalActualizada.xlsx",
    sheet = 1,
    skip = 2
  ) |>
    janitor::clean_names() |>
    select(
      punto = 1,
      id = 2,
      sitio = 3,
      latitud = 4,
      longitud = 5,
      fecha = 6,
      temperatura = 14,
      pH = 15,
      od = 16,
      ds = 17,
      ce = 19,
      sdt = 20,
      turb = 21,
      cla = 26,
      cla_ciano = 27
    ) |>
    mutate(
      across(
        .cols = c(punto:id, latitud:longitud, temperatura:cla_ciano),
        .fns = as.numeric
      )
    ) |>
    mutate(fecha = as.Date(fecha)) |>
    mutate(latitud = if_else(latitud < 0, latitud, -latitud)) |>
    mutate(longitud = if_else(longitud < 0, longitud, -longitud)) |>
    pivot_longer(
      cols = temperatura:cla_ciano,
      values_to = "valor",
      names_to = "param"
    ) |>
    full_join(parametros_tbl, by = join_by(param)) |>
    full_join(sitio_tbl, by = join_by(sitio)) |>
    drop_na(valor) |>
    fill(fecha) |>
    write_csv("datos/base_de_datos.csv")
}
