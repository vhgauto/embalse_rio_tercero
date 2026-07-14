# paquetes ---------------------------------------------------------------

library(tidymodels)
library(ggspatial)
library(tidyterra)

# mes_actual <- 12
# año_actual <- 2025

# source("scripts/.soporte.R")

# datos ------------------------------------------------------------------

p <- filter(d, fecha == max(d$fecha, na.rm = TRUE) & param == "ds") |>
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326") |>
  project(crs(r_agua))

ds_tbl <- terra::extract(r_agua, p) |>
  as_tibble() |>
  select(-fmask, -ID)

nombres_ds <- names(ds_tbl)

# modelo -----------------------------------------------------------------

ds_tbl |>
  mutate(ds = p$valor) |>
  corrr::correlate(
    use = "pairwise.complete.obs",
    method = "pearson",
    quiet = TRUE
  ) |>
  filter(term == "ds") |>
  pivot_longer(cols = -term) |>
  drop_na() |>
  arrange(desc(value))

rr <- ds_tbl |>
  mutate(ds = p$valor)

index_tbl <- expand_grid(
  var1 = 1:length(nombres_ds),
  var2 = 1:length(nombres_ds)
) |>
  filter(var1 != var2) |>
  mutate(df = list(ds_tbl), .before = 1)

ratio_tbl <- pmap(index_tbl, f_ratio) |>
  list_cbind()

ratio_tbl |>
  mutate(ds = p$valor, .before = 1) |>
  corrr::correlate(
    use = "pairwise.complete.obs",
    method = "pearson",
    quiet = TRUE
  ) |>
  filter(term == "ds") |>
  pivot_longer(cols = -term) |>
  drop_na() |>
  arrange(desc(value))

mod_tbl <- terra::extract(r_agua, p) |>
  as_tibble() |>
  select(-fmask, -ID) |>
  mutate(ds = p$valor, .before = 1) |>
  mutate(ratio = green / red + blue / red) |> # <---- mejor cociente de bandas
  select(ds, ratio)

# workflow
base_wf <- workflow() |>
  add_formula(ds ~ .)

# especificación
lm_spec <- linear_reg() |>
  set_engine("lm")

# modelos
mod_lm <- base_wf |>
  add_model(lm_spec) |>
  fit(mod_tbl) # <---------- uso un cociente (mod_tbl) ó todas las bandas (rr)

# verifico
glance(mod_lm)

# ráster a tbl
r_agua_tbl <- terra::as.data.frame(r_agua) |>
  as_tibble() |>

  # filter(nir != 0) |>

  transmute(ratio = green / red + blue / red) # <-------- cociente de bandas

pred_tbl <- predict(
  extract_fit_engine(mod_lm),
  r_agua_tbl
) |>
  tibble(.pred = _)

predict_ds <- terra::as.data.frame(r_agua$nir, xy = TRUE) |>
  as_tibble() |>
  # dplyr::filter(nir != 0) |>
  bind_cols(pred_tbl) |>
  select(-nir) |>
  mutate(.pred = if_else(.pred <= 0, 0, .pred)) |>
  rast()

predict_ds[is.infinite(predict_ds)] <- NA

tr <- thresh(predict_ds, as.raster = FALSE, method = "otsu")
tr
max(p$valor)

# histograma
ggplot() +
  geom_histogram(
    data = as.data.frame(predict_ds),
    aes(x = .pred),
    binwidth = 1
  ) +
  geom_vline(xintercept = tr, color = "red", linewidth = 1) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  coord_cartesian(xlim = c(0, tr * 1)) +
  theme_bw(base_size = 4)

# remuevo valores extremos
lim_transparencia <- 9
predict_ds[predict_ds$.pred > lim_transparencia] <- lim_transparencia

# mapa -------------------------------------------------------------------

fecha_transparencia <- ymd(sub(".rds", "", basename(r_actual)))

etq_transparencia <- paste0(
  "Producto HSL\nFecha: ",
  ymd(fecha_transparencia),
  "\nReflectancia de superficie"
)

mapa_transparencia <- ggplot() +
  geom_spatraster(data = predict_ds, interpolate = FALSE) +
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
    label = etq_transparencia,
    size = 3,
    family = "Arial",
    hjust = 1,
    vjust = 0,
    lineheight = .8
  ) +
  scale_fill_hypso_c(palette = "meyers", direction = -1) +
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
  labs(fill = "Transparencia del agua<br>según disco de Secchi (m)") +
  theme_void(base_size = 8, base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    legend.key.height = unit(7, "pt"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.text = element_text(size = 6, margin = margin(t = 2)),
    legend.title = ggtext::element_markdown(
      margin = margin(t = 0, b = 2, r = 15),
      lineheight = 1
    )
  )

# guardo -----------------------------------------------------------------

guardar_png(
  plot = mapa_transparencia,
  filename = "mapa_transparencia",
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
    "/mapa_transparencia_",
    año_actual,
    "_",
    mes_actual_chr,
    ".tif"
  ))
}

extract_ds <- terra::extract(predict_ds, v) |>
  as_tibble() |>
  rename(punto = ID)

r2_ds <- dplyr::filter(d, fecha == max(d$fecha) & param == "ds") |>
  select(punto, valor) |>
  inner_join(extract_ds, by = join_by(punto)) |>
  lm(valor ~ .pred, data = _) |>
  broom::glance() |>
  pull(r.squared)
