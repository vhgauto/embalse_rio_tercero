d <- read_csv("datos/d.csv", show_col_types = FALSE) |>
  mutate(unidad = if_else(is.na(unidad), "", unidad))

# estadísticos
fns_labels <- list(
  Media = ~ mean(., na.rm = TRUE),
  `Desvío estándar` = ~ sd(., na.rm = TRUE),
  Mínimo = ~ min(., na.rm = TRUE),
  Máximo = ~ max(., na.rm = TRUE)
)

# tabla
tabla <- filter(d, month(fecha) == 6 & year(fecha) == 2025) |>
  select(sitio_etq, param, valor, unidad, param_etq) |>
  mutate(param_etq = paste0("**", param_etq, "**")) |>
  mutate(
    unidad = if_else(
      param == "pH",
      "",
      paste0("(", unidad, ")")
    )
  ) |>
  rowwise() |>
  mutate(param_label = paste0(param_etq, "\n", unidad)) |>
  ungroup() |>
  pivot_wider(
    names_from = param_label,
    values_from = valor,
    id_cols = sitio_etq
  ) |>
  mutate(grupo = "Sitio de muestreo") |>
  gt(rowname_col = "sitio_etq", groupname_col = "grupo") |>
  summary_rows(
    columns = 2:10,
    fns = fns_labels,
    fmt = ~ fmt_number(., decimals = 1, sep_mark = ".", dec_mark = ",")
  ) |>
  tab_style(
    style = cell_text(
      weight = "bold",
      transform = "capitalize",
      align = "right"
    ),
    locations = cells_stub_summary(
      groups = "Sitio de muestreo"
    )
  ) |>
  fmt_number(
    columns = everything(),
    decimals = 1,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  fmt_number(
    columns = contains("pH"),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  cols_label_with(columns = everything(), fn = md) |>
  tab_style(
    locations = cells_column_labels(),
    style = cell_text(v_align = "middle", align = "center")
  ) |>
  sub_missing(missing_text = "---") |>
  cols_label(
    sitio_etq = md("**Sitio de muestreo**")
  ) |>
  tab_header(
    title = md(
      "Variables medidas en el embalse Río Tercero durante la campaña de muestreo realizada en **Junio de 2025**"
    )
  ) |>
  tab_footnote(
    footnote = "Temp.: Temperatura del agua, OD: Oxígeno disuelto, DS: Transparencia del agua según la profundidad del disco de Secchi, CE: Conductividad eléctrica, SDT: Sólidos disueltos totales, Turb: Turbiedad, Cl-a: Concentración de clorofila-a. LE Sitios de muestreo ubicados en el lóbulo Este del embalse. LO Sitios de muestreo del lóbulo Oeste"
  ) |>
  tab_options(
    # summary_row.background.color = "grey95",
    # table.font.names = "Times New Roman",
    # table.font.size = "8px",
    # heading.title.font.size = "8px"
  )

tabla_parametros <- filter(
  d,
  month(fecha) == mes_actual & year(fecha) == año_actual
) |>
  select(sitio_etq, param, valor, unidad, param_etq) |>
  mutate(param_etq = paste0("**", param_etq, "**")) |>
  mutate(
    unidad = if_else(
      param == "pH",
      "",
      paste0("(", unidad, ")")
    )
  ) |>
  pivot_wider(
    names_from = param,
    values_from = valor,
    id_cols = sitio_etq
  ) |>
  # mutate(across(.cols = everything(), .fns = as.character)) |>
  # mutate(across(.cols = everything(), .fns = ~ replace_na(.x, "-"))) |>
  rename("temp" = "temperatura", "cla_c" = "cla_ciano")

tabla_parametros_chr <- tabla_parametros |>
  mutate(across(.cols = everything(), .fns = as.character)) |>
  mutate(across(.cols = everything(), .fns = ~ replace_na(.x, "-")))

tabla_parametros_resumen <- tabla_parametros |>
  pivot_longer(
    cols = -sitio_etq,
    values_to = "valor",
    names_to = "param"
  ) |>
  reframe(
    MEDIA = mean(valor, na.rm = TRUE),
    "DESVÍO ESTÁNDAR" = sd(valor, na.rm = TRUE),
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
  mutate(across(.cols = -sitio_etq, .fns = ~ round(., 1))) |>
  mutate(sitio_etq = paste0("**", sitio_etq, "**")) |>
  mutate(across(.cols = -sitio_etq, .fns = as.character))

options(knitr.kable.NA = "-")

tabla_parametros_unidad <- tibble(
  sitio_etq = "**Unidades**",
  temp = "°C",
  pH = "-",
  od = "mg/L",
  ds = "m",
  sdt = "ppm",
  turb = "UNT",
  ce = "µS/cm",
  cla = "mg/m^3",
  cla_c = "mg/m^3"
)

tabla_parametros_md <- bind_rows(
  tabla_parametros_unidad,
  tabla_parametros_chr,
  tabla_parametros_resumen
) |>
  mutate(sitio_etq = str_remove(sitio_etq, "río ")) |>
  knitr::kable()
