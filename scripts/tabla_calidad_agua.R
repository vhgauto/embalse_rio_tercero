protocolos <- list.files(
  paste0("datos/calidad_agua_", año_actual, "_", mes_actual_chr),
  pattern = "protocolo",
  full.names = TRUE
)

sitio9 <- protocolos[str_detect(protocolos, "9")]
sitio11 <- protocolos[str_detect(protocolos, "11")]

d9 <- f_pdf1(sitio9)
d11 <- f_pdf1(sitio11)

d_limite <- tabulapdf::extract_tables(
  "datos/calidad_agua_2025_10/protocolo análisis agua-Dr. Bonansea-sitio 11.pdf",
  pages = 2,
  method = "stream"
) |>
  pluck(1) |>
  select("param" = 1, "limite" = 5) |>
  drop_na(param) |>
  mutate(limite = replace_na(limite, "-")) |>
  mutate(param = str_replace(param, "(.+)\\: .+", "\\1")) |>
  mutate(param = str_replace(param, "oC", "°C")) |>
  mutate(limite = str_replace_all(limite, ",", "."))

nit_l <- list.files(
  paste0("datos/calidad_agua_", año_actual, "_", mes_actual_chr),
  pattern = "W",
  full.names = TRUE
)

# tabulapdf::extract_text(nit_l[str_detect(nit_l, "11")]) |>
#   str_split("\n") |>
#   pluck(1) |>
#   str_replace_all("\r", "")

tabla_calidad_tbl <- inner_join(d9, d11, by = join_by(param, unidad)) |>
  relocate(1, 3, 2, 4) |>
  rename("SM 9: Centro" = 3, "SM 11: Presa" = 4) |>
  inner_join(d_limite, by = join_by(param)) |>
  mutate(
    limite = if_else(
      unidad == "-",
      limite,
      str_extract(limite, "^\\S+\\s+\\S+")
    )
  ) |>
  mutate(limite = replace_na(limite, "-")) |>
  rename(
    "Determinación" = param,
    "Unidad" = unidad,
    "Límite de aptitud" = limite
  ) |>
  filter(!Determinación %in% c("Conductividad a 25 °C", "Nitrato"))

nit_l <- list.files(
  paste0("datos/calidad_agua_", año_actual, "_", mes_actual_chr),
  pattern = "W",
  full.names = TRUE
)

nit9 <- f_pdf2(9)
nit11 <- f_pdf2(11)

nit_tbl <- tibble(
  "1" = c("Nitrógeno Kjeldahl", "Nitratos", "Fósforo Total"),
  "2" = "mg/L",
  "3" = nit9,
  "4" = nit11,
  "5" = "-"
)

names(nit_tbl) <- names(tabla_calidad_tbl)

tabla_calidad_md <- knitr::kable(
  bind_rows(tabla_calidad_tbl, nit_tbl),
  col.names = paste0("**", names(tabla_calidad_tbl), "**")
)
