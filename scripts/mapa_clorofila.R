# paquetes ---------------------------------------------------------------

library(tidymodels)
library(ggspatial)
library(tidyterra)

# mes_actual <- 12
# año_actual <- 2025

# source("scripts/.soporte.R")

# datos ------------------------------------------------------------------

p <- filter(d, fecha == max(d$fecha, na.rm = TRUE) & param == "cla") |>
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326") |>
  project(crs(r_agua))

cla_tbl <- terra::extract(r_agua, p) |>
  as_tibble() |>
  select(-fmask, -ID)

nombres_cla <- names(cla_tbl)

# modelo -----------------------------------------------------------------

cla_tbl |>
  mutate(cla = p$valor) |>
  corrr::correlate(
    use = "pairwise.complete.obs",
    method = "pearson",
    quiet = TRUE
  ) |>
  filter(term == "cla") |>
  pivot_longer(cols = -term) |>
  drop_na() |>
  arrange(desc(value))

rr <- cla_tbl |>
  mutate(cla = p$valor)

lm(cla ~ ., data = rr) |>
  glance()

index_tbl <- expand_grid(
  var1 = 1:length(nombres_cla),
  var2 = 1:length(nombres_cla)
) |>
  filter(var1 != var2) |>
  mutate(df = list(cla_tbl), .before = 1)

nombres_ds <- nombres_cla

ratio_tbl <- pmap(index_tbl, f_ratio) |>
  list_cbind()

ratio_tbl |>
  mutate(cla = p$valor, .before = 1) |>
  corrr::correlate(
    use = "pairwise.complete.obs",
    method = "pearson",
    quiet = TRUE
  ) |>
  filter(term == "cla") |>
  pivot_longer(cols = -term) |>
  drop_na() |>
  arrange(desc(value))

mod_tbl <- terra::extract(r_agua, p) |>
  as_tibble() |>
  select(-fmask, -ID) |>
  mutate(cla = p$valor, .before = 1) |>
  mutate(ratio = nir / blue) |> # <-------- cociente de bandas
  select(cla, ratio)

# workflow
base_wf <- workflow() |>
  add_formula(cla ~ .)
# add_recipe()

# especificación
lm_spec <- linear_reg() |>
  set_engine("lm")

# modelos
mod_lm <- base_wf |>
  add_model(lm_spec) |>
  fit(rr) # <---------- uso un cociente (mod_tbl) ó todas las bandas (rr)

# verifico
glance(mod_lm)

# ráster a tbl
r_agua_tbl <- terra::as.data.frame(r_agua) |>
  as_tibble() #|>

# filter(blue != 0) |>

# transmute(ratio = nir / blue)

pred_tbl <- predict(
  extract_fit_engine(mod_lm),
  r_agua_tbl
) |>
  tibble(.pred = _)

predict_cla <- terra::as.data.frame(r_agua$blue, xy = TRUE) |>
  as_tibble() |>

  # filter(blue != 0) |>

  bind_cols(pred_tbl) |>
  select(-blue) |>
  mutate(.pred = if_else(.pred <= 0, 0, .pred)) |>
  rast()

tr <- thresh(predict_cla, as.raster = FALSE, method = "mean")
tr
max(p$valor)

# histograma
ggplot() +
  geom_histogram(
    data = as.data.frame(predict_cla),
    aes(x = .pred),
    binwidth = 1
  ) +
  geom_vline(xintercept = tr, color = "red", linewidth = 1) +
  geom_vline(
    xintercept = max(p$valor),
    color = "blue",
    linewidth = .3,
    linetype = 2
  ) +
  scale_x_continuous(breaks = scales::breaks_width(5)) +
  coord_cartesian(xlim = c(0, tr * 5)) +
  theme_bw(base_size = 4)

# remuevo valores extremos
lim_clorofila <- 15
predict_cla[predict_cla$.pred > lim_clorofila] <- lim_clorofila

# mapa -------------------------------------------------------------------

fecha_clorofila <- ymd(sub(".rds", "", basename(r_actual)))

etq_clorofila <- paste0(
  "Producto HSL\nFecha: ",
  ymd(fecha_clorofila),
  "\nReflectancia de superficie"
)

mapa_clorofila <- ggplot() +
  geom_spatraster(data = predict_cla, interpolate = FALSE) +
  geom_segment(
    data = flechas_tbl,
    aes(x = xi, y = yi, xend = xf, yend = yf, group = id),
    arrow = arrow(angle = 10, length = unit(2, "mm"), type = "closed"),
    linewidth = .2
  ) +
  annotate(
    geom = "text",
    x = I(.99),
    y = I(.01),
    label = etq_clorofila,
    size = 3,
    family = "Arial",
    hjust = 1,
    vjust = 0,
    lineheight = .8
  ) +
  scale_fill_grass_c(palette = "grass", direction = -1) +
  annotation_north_arrow(
    location = "tl",
    style = north_arrow_fancy_orienteering(),
    height = unit(.8, "cm"),
    width = unit(.8, "cm"),
    pad_x = unit(0.25, "cm"),
    pad_y = unit(0.25, "cm")
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    width_hint = .1,
    text_family = "Arial"
  ) +
  coord_sf(expand = FALSE) +
  labs(fill = "Concentración de<br>clorofila-a (mg/m<sup>3</sup>)") +
  theme_void(base_size = 8, base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    legend.key.height = unit(7, "pt"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.text = element_text(size = 6, margin = margin(t = 2)),
    legend.title = ggtext::element_markdown(
      margin = margin(t = 0, b = 2, r = 15)
    )
  )

# guardo -----------------------------------------------------------------

guardar_png(
  plot = mapa_clorofila,
  filename = "mapa_clorofila",
  ancho = 5,
  alto = 5,
  formato = ".tif"
)

if (FALSE) {
  browseURL(paste0(
    "fig/",
    año_actual,
    "-",
    mes_actual_chr,
    "/mapa_clorofila_",
    año_actual,
    "_",
    mes_actual_chr,
    ".tif"
  ))
}

extract_cla <- terra::extract(predict_cla, v) |>
  as_tibble() |>
  rename(punto = ID)

r2_cla <- filter(d, fecha == max(d$fecha) & param == "cla") |>
  select(punto, valor) |>
  inner_join(extract_cla, by = join_by(punto)) |>
  lm(valor ~ .pred, data = _) |>
  broom::glance() |>
  pull(r.squared)
