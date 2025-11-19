# https://cordoba.redesclimaticas.com/next/reports?mss=30335

a1 <- readxl::read_xlsx(
  "datos/Base_ERT_OriginalActualizada.xlsx",
  sheet = "3-Nivel_Diques-Cba"
) |>
  select(fecha = 1, altura = "Embalse") |>
  filter(fecha != "h labio de Vertedero") |>
  drop_na() |>
  mutate(fecha = janitor::excel_numeric_to_date(as.numeric(fecha)))

a2 <- readxl::read_xlsx(
  "datos/resumenes_diarios_2025-09-01_2025-10-01.xlsx",
  skip = 5
) |>
  select(fecha = 1, altura = 3) |>
  mutate(fecha = dmy(fecha))

rbind(a1, a2) |>
  arrange(desc(fecha)) |>
  mutate(año = year(fecha)) |>
  filter(año == 2025) |>
  mutate(mes = month(fecha)) |>
  count(mes)

if (FALSE) {
  rbind(a1, a2) |>
    arrange(desc(fecha)) |>
    write_csv("datos/base_de_datos_cotas.csv")
}

#

# XXX LECTURA DE DATOS

# cotas_l <- list.files(
#   "datos/",
#   pattern = "resumenes_diarios_",
#   full.names = TRUE
# )

# f_cota <- function(H) {
#   readxl::read_xlsx(H, skip = 5) |>
#     select(fecha = 1, altura = 3) |>
#     mutate(fecha = dmy(fecha))
# }

# cota_actualizado <- map(cotas_l, f_cota) |>
#   list_rbind()

# read_csv("datos/base_de_datos_cotas.csv", show_col_types = FALSE) |>
#   rbind(cota_actualizado) |>
#   distinct()
