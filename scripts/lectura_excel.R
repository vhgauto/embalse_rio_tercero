library(tidyverse)

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
  "S.5 = HOTELES",
  "Garganta",
  "Entrada Rios y CNE",
  "S.2 = CANAL ENFRIAMIENTO",
  "S.1 = CONFLUENCIA RIOS",
  "entrada rio Grande",
  "rio Santa Rosa",
  "S.4 = CENTRO",
  "S.6 = VILLA DEL DIQUE",
  "S.7 = MURALLON"
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

# if (FALSE) {
#   d1 <- readxl::read_xlsx(path = "datos/Base_ERT_OriginalActualizada.xlsx") |>
#     janitor::clean_names() |>
#     slice(3:24) |>
#     select(
#       punto = x1,
#       id,
#       sitio = sitio_de_muestreo,
#       latitud = x5,
#       longitud = fecha,
#       fecha = hora,
#       temperatura = temp_oh2,
#       pH = p_h,
#       od,
#       ds = disco_s,
#       sdt = soliddos_disueltos_totales,
#       turb = turbidez,
#       ce = conductividad,
#       cla = cloroflila_total_sonda,
#       cla_ciano = clorofila_ciano_sonda
#     ) |>
#     mutate(
#       across(
#         .cols = c(punto, latitud:cla_ciano),
#         .fns = as.numeric
#       )
#     ) |>
#     mutate(fecha = janitor::excel_numeric_to_date(fecha)) |>
#     mutate(latitud = if_else(latitud < 0, latitud, -latitud)) |>
#     mutate(longitud = if_else(longitud < 0, longitud, -longitud)) |>
#     pivot_longer(
#       cols = temperatura:cla_ciano,
#       values_to = "valor",
#       names_to = "param"
#     ) |>
#     full_join(parametros_tbl, by = join_by(param)) |>
#     full_join(sitio_tbl, by = join_by(sitio))
# }

# if (FALSE) {
#   d2 <- readxl::read_xlsx(
#     path = "datos/Base_ERT_OriginalActualizada.xlsx",
#     sheet = 1
#   ) |>
#     janitor::clean_names() |>
#     slice(25:1e5) |>
#     select(
#       punto = x1,
#       id,
#       sitio = sitio_de_muestreo,
#       latitud = coordenadas_decimales,
#       longitud = x5,
#       fecha,
#       temperatura = temp_oh2,
#       pH = p_h,
#       od,
#       ds = disco_s,
#       sdt = soliddos_disueltos_totales,
#       turb = turbidez,
#       ce = conductividad,
#       cla = cloroflila_total_sonda,
#       cla_ciano = clorofila_ciano_sonda
#     ) |>
#     names()
#   mutate(
#     across(
#       .cols = c(punto, latitud:cla_ciano),
#       .fns = as.numeric
#     )
#   ) |>
#     mutate(fecha = janitor::excel_numeric_to_date(fecha)) |>
#     mutate(latitud = if_else(latitud < 0, latitud, -latitud)) |>
#     mutate(longitud = if_else(longitud < 0, longitud, -longitud)) |>
#     pivot_longer(
#       cols = temperatura:cla_ciano,
#       values_to = "valor",
#       names_to = "param"
#     ) |>
#     full_join(parametros_tbl, by = join_by(param)) |>
#     full_join(sitio_tbl, by = join_by(sitio))
# }

# if (FALSE) {
#   write_csv(rbind(d1, d2), "datos/base_de_datos.csv")
# }

# google sheet
if (FALSE) {
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
      temperatura = 13,
      pH = 14,
      od = 15,
      ds = 16,
      ce = 18,
      sdt = 19,
      turb = 20,
      cla = 25,
      cla_ciano = 26
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
