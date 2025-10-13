library(tidyverse)

# https://www.cba.gov.ar/nivel-de-diques-y-embalses/

f_dique <- function(x) {
  d_ene <- readxl::read_xlsx(
    "datos/medicion-diques-2025-ENERO.xlsx",
    sheet = x,
    skip = 1
  ) |>
    select("dique" = 1, "altura" = 2, "vertedero" = 3) |>
    filter(dique == "Embalse")

  d_fecha <- readxl::read_xlsx(
    "datos/medicion-diques-2025-ENERO.xlsx",
    sheet = x,
    range = "A1:A2"
  )

  print(x)

  tibble(
    fecha = janitor::excel_numeric_to_date(as.numeric(names(d_fecha))),
    altura = d_ene$altura,
    vertedero = d_ene$vertedero
  )
}

dique_list <- map(
  1:995,
  ~ readxl::read_xlsx(
    "datos/medicion-diques-2025-ENERO.xlsx",
    sheet = .x,
    progress = FALSE
  )
)

dique_list2 <- tibble(
  l = dique_list
) |>
  mutate(n = map_dbl(l, nrow)) |>
  filter(n != 0) |>
  pull(l)

dique_altura <- map(
  dique_list2,
  \(A) {
    i <- which(A[, 1] == "Embalse")
    pull(A[6, 2])
  }
)

tibble(
  l = dique_list2
) |>
  mutate(id = row_number()) |>
  mutate(
    altura = map(
      l,
      \(A) {
        i <- which(A[, 1] == "Embalse")
        as.character(pull(A[6, 2]))
      }
    )
  ) |>
  unnest(altura) |>
  mutate(altura = parse_number(altura)) |>
  drop_na() |>
  mutate(
    fecha = map_chr(
      l,
      ~ names(.x)[1]
    )
  ) |>
  mutate(
    fecha = case_when(
      nchar(fecha) == 13 ~ str_remove(fecha, "DÍA: "),
      nchar(fecha) == 12 ~ str_remove(fecha, "DÍA: "),
      nchar(fecha) == 11 ~ str_remove(fecha, "DÍA: "),
      nchar(fecha) == 5 ~
        as.numeric(fecha) |>
          janitor::excel_numeric_to_date() |>
          as.character(),
      .default = NA
    )
  ) |>
  drop_na() |>
  mutate(fecha = ymd(fecha)) |>
  ggplot(aes(fecha, altura)) +
  geom_line(alpha = .7) +
  geom_point(alpha = .7) +
  theme(
    aspect.ratio = .4
  )


length(dique_altura)

fechas1 <- map(dique_list2, ~ names(.x)[1])

fechas1[1:10]

tibble(
  fe = fechas1
) |>
  mutate(n = map_dbl(fe, nchar)) |>
  count(n, sort = TRUE)


tibble(
  fe = fechas1
) |>
  mutate(id = row_number()) |>
  mutate(ch = map_dbl(fe, nchar)) |>
  filter(ch == 17) |>
  unnest(fe)

dique_list2[[656]]
