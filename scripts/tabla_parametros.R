tabla_parametros <- filter(
  d,
  month(fecha) == mes_actual & year(fecha) == año_actual
) |>
  select(sitio_etq, param, valor, unidad, param_etq) |>
  mutate(
    param_etq = if_else(param == "cla_ciano", "Ciano.", param_etq)
  ) |>
  mutate(param_etq = paste0("**", param_etq, "**")) |>
  mutate(
    unidad = if_else(
      param == "pH",
      "",
      paste0("(", unidad, ")")
    )
  ) |>
  pivot_wider(
    names_from = param_etq,
    values_from = valor,
    id_cols = sitio_etq
  )

tabla_parametros_chr <- tabla_parametros |>
  mutate(across(.cols = -sitio_etq, .fns = formato)) |>
  mutate(across(
    .cols = -sitio_etq,
    .fns = ~ if_else(str_detect(., "NA"), "-", .)
  ))

tabla_parametros_resumen <- tabla_parametros |>
  pivot_longer(
    cols = -sitio_etq,
    values_to = "valor",
    names_to = "param"
  ) |>
  reframe(
    MEDIA = mean(valor, na.rm = TRUE),
    "DESV. ESTÁNDAR" = sd(valor, na.rm = TRUE),
    MÍNIMO = min(valor, na.rm = TRUE),
    MÁXIMO = max(valor, na.rm = TRUE),
    .by = param
  ) |>
  pivot_longer(
    cols = -param,
    values_to = "valor",
    names_to = "sitio_etq"
  ) |>
  pivot_wider(names_from = param, values_from = valor) |>
  mutate(across(.cols = -sitio_etq, .fns = formato)) |>
  mutate(sitio_etq = paste0("**", sitio_etq, "**")) |>
  mutate(across(.cols = -sitio_etq, .fns = as.character))

unidades_v <- c(
  "**Unidades**",
  "°C",
  "-",
  "mg/L",
  "m",
  "µS/cm",
  "ppm",
  "UNT",
  "mg/m³",
  "mg/m³"
)

tabla_parametros_unidad <- tibble(
  u = unidades_v,
  n = names(tabla_parametros_resumen)
) |>
  pivot_wider(
    names_from = n,
    values_from = u
  )

tabla_parametros_md <- bind_rows(
  tabla_parametros_unidad,
  tabla_parametros_chr,
  tabla_parametros_resumen
) |>
  mutate(sitio_etq = str_remove(sitio_etq, "río ")) |>
  rename(" " = sitio_etq) |>
  knitr::kable()
