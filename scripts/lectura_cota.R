# https://cordoba.redesclimaticas.com/next/reports?mss=30335

library(tidyverse)

lista_xlsx <- list.files(
  "datos/altura_cota/",
  pattern = "resumenes_diarios",
  full.names = TRUE
)

f_cota <- function(H) {
  readxl::read_xlsx(H, skip = 5) |>
    select(fecha = 1, altura = 3) |>
    mutate(fecha = dmy(fecha))
}

cota_tbl <- map(lista_xlsx, f_cota) |>
  list_rbind()

base_de_datos_cota <- read_csv(
  "datos/base_de_datos_cotas.csv",
  show_col_types = FALSE
)

if (FALSE) {
  rbind(base_de_datos_cota, cota_tbl) |>
    arrange(desc(fecha)) |>
    distinct() |>
    write_csv("datos/base_de_datos_cotas.csv")
}
