f_pdf <- function(archivo) {
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

d9 <- f_pdf(
  "datos/protocolo análisis agua-Embalsr rio III-Sitio 9-Bonansea.pdf"
)
d11 <- f_pdf(
  "datos/protocolo análisis agua-Embalsr rio III-Sitio 11-Bonansea.pdf"
)

d_limite <- tabulapdf::extract_tables(
  "datos/protocolo análisis agua-Embalsr rio III-Sitio 11-Bonansea.pdf",
  pages = 2,
  method = "stream"
) |>
  pluck(1) |>
  select("param" = 1, "limite" = 5) |>
  drop_na(param) |>
  mutate(limite = replace_na(limite, "-")) |>
  mutate(param = str_replace(param, "(.+)\\: .+", "\\1")) |>
  mutate(param = str_replace(param, "oC", "°C"))

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
  mutate(across(
    .cols = c("SM 9: Centro", "SM 11: Presa"),
    .fns = ~ str_replace_all(., "\\.", ",")
  ))

tabla_calidad_md <- knitr::kable(
  tabla_calidad_tbl,
  col.names = paste0("**", names(tabla_calidad_tbl), "**")
)
