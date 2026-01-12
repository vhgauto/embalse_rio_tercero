library(tidyverse)

gen_diatomeas <- c(
  "Aulacoseira",
  "Cyclotela",
  "Asterionella",
  "Fragillaria",
  "Navicula",
  "Coconeis",
  "Nistchia",
  "Gyrosigma",
  "Cymbella",
  "Stephanodiscus",
  "Cymatopleura"
)

gen_clorofitas <- c(
  "Staurastrum",
  "Closterium",
  "Oocystis",
  "Pediastrum",
  "Ulothrix",
  "Spirogyra"
)

gen_ciano <- c("Dolichospermum", "Microcystis", "Nostoc")

gen_criptofitas <- c("Ceratium", "Peridinium")

mes_actual_chr <- if (mes_actual <= 9) paste0("0", mes_actual) else mes_actual

ex <- list.files(
  path = paste0("datos/algas_", año_actual, "_", mes_actual_chr),
  full.names = TRUE
)

archivo <- list.files(
  path = paste0(
    "datos/algas_",
    año_actual,
    "_",
    mes_actual_chr
  ),
  full.names = TRUE
)

f_micro <- function(archivo, W) {
  ctes_tbl <- readxl::read_xlsx(
    path = ex,
    sheet = W,
    range = "A1:B5"
  ) |>
    rename(param = 1, valor = 2)

  sitio_int <- readxl::read_xlsx(
    path = ex,
    sheet = W,
    range = "F1:F2"
  ) |>
    names() |>
    as.integer()

  n_campos <- ctes_tbl[ctes_tbl$param == "N° Campos", 2]$valor
  vol_filtrado <- ctes_tbl[ctes_tbl$param == "Vol Filtrado mL (V)", 2]$valor
  area_filtrado <- ctes_tbl[
    ctes_tbl$param == "Area total defiltración mm2 (At)",
    2
  ]$valor
  area_campo <- ctes_tbl[
    ctes_tbl$param == "Area total de un campo mm2 (Ac)",
    2
  ]$valor

  readxl::read_xlsx(
    path = archivo,
    sheet = W,
    skip = 5
  ) |>
    janitor::clean_names() |>
    select(generos, starts_with("campo"), factor_celular) |>
    filter(
      generos %in% c(gen_diatomeas, gen_clorofitas, gen_ciano, gen_criptofitas)
    ) |>
    mutate(
      across(
        .cols = c(starts_with("campo"), factor_celular),
        .fns = as.numeric
      )
    ) |>
    pivot_longer(
      names_to = "campo",
      values_to = "conteo",
      cols = starts_with("campo")
    ) |>
    mutate(conteo = replace_na(conteo, 0)) |>
    mutate(factor_celular = replace_na(factor_celular, 1)) |>
    reframe(
      subtotal = sum(conteo),
      .by = c(generos, factor_celular)
    ) |>
    mutate(
      indiv_ml = (subtotal * area_filtrado) /
        (area_campo * n_campos * vol_filtrado)
    ) |>
    mutate(cel_litro = round(indiv_ml * factor_celular * 1000)) |>
    mutate(sitio = sitio_int)
}

algas_orden <- c("Diatomeas", "Clorofitas", "Cianobacterias", "Criptófitas")

d_micro_actual <- read_csv(
  "datos/base_de_datos_micro.csv",
  show_col_types = FALSE
)

if (FALSE) {
  d_micro <- map_dfr(1:10, ~ f_micro(archivo, .x)) |>
    mutate(
      algas = case_when(
        generos %in% gen_diatomeas ~ "Diatomeas",
        generos %in% gen_criptofitas ~ "Criptófitas",
        generos %in% gen_ciano ~ "Cianobacterias",
        generos %in% gen_clorofitas ~ "Clorofitas"
      )
    ) |>
    mutate(algas = factor(algas, levels = algas_orden)) |>
    reframe(
      suma = sum(cel_litro),
      .by = c(sitio, algas)
    ) |>
    mutate(fecha = max(d$fecha), .before = 1)

  write_csv(rbind(d_micro_actual, d_micro), "datos/base_de_datos_micro.csv")
}
