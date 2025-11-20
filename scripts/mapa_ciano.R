# paquetes ---------------------------------------------------------------

library(tidymodels)
library(ggspatial)
library(tidyterra)

# datos ------------------------------------------------------------------

p <- filter(d, fecha == max(d$fecha, na.rm = TRUE) & param == "cla_ciano") |>
  vect(geom = c("longitud", "latitud"), crs = "EPSG:4326") |>
  project(crs(r_agua))

ciano_tbl <- terra::extract(r_agua, p) |>
  as_tibble() |>
  select(-fmask, -ID)

nombres_ciano <- names(ciano_tbl)

nombres_ds <- names(ds_tbl)

# modelo -----------------------------------------------------------------

ciano_tbl |>
  mutate(ciano = p$valor) |>
  corrr::correlate(
    use = "pairwise.complete.obs",
    method = "pearson",
    quiet = TRUE
  ) |>
  filter(term == "ciano") |>
  pivot_longer(cols = -term) |>
  drop_na() |>
  arrange(desc(value))

rr <- ciano_tbl |>
  mutate(ciano = p$valor)

lm(ciano ~ ., data = rr) |>
  glance()

index_tbl <- expand_grid(
  var1 = 1:length(nombres_ciano),
  var2 = 1:length(nombres_ciano)
) |>
  filter(var1 != var2) |>
  mutate(df = list(ciano_tbl), .before = 1)

ratio_tbl <- pmap(index_tbl, f_ratio) |>
  list_cbind()

ratio_tbl |>
  mutate(ciano = p$valor, .before = 1) |>
  corrr::correlate(
    use = "pairwise.complete.obs",
    method = "pearson",
    quiet = TRUE
  ) |>
  filter(term == "ciano") |>
  pivot_longer(cols = -term) |>
  drop_na() |>
  arrange(desc(value))

mod_tbl <- terra::extract(r_agua, p) |>
  as_tibble() |>
  select(-fmask, -ID) |>
  mutate(ciano = p$valor, .before = 1) |>
  mutate(ratio = swir1 / green) |>
  select(ciano, ratio)

# workflow
base_wf <- workflow() |>
  add_formula(ciano ~ .)
# add_recipe()

# especificación
lm_spec <- linear_reg() |>
  set_engine("lm")

# modelos
mod_lm <- base_wf |>
  add_model(lm_spec) |>
  fit(rr)

# verifico
glance(mod_lm)

# ráster a tbl
r_agua_tbl <- terra::as.data.frame(r_agua) |>
  as_tibble() #|>
# transmute(ratio = swir2 / green)

pred_tbl <- predict(
  extract_fit_engine(mod_lm),
  r_agua_tbl
) |>
  tibble(.pred = _)

predict_ciano <- terra::as.data.frame(r_agua$nir, xy = TRUE) |>
  as_tibble() |>
  bind_cols(pred_tbl) |>
  select(-nir) |>
  mutate(.pred = if_else(.pred <= 0, 0, .pred)) |>
  rast()

# histograma
ggplot() +
  geom_histogram(
    data = as.data.frame(predict_ciano),
    aes(x = .pred),
    binwidth = 1
  ) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  coord_cartesian(xlim = c(0, 20)) +
  theme_bw(base_size = 4)

# remuevo valores extremos
lim_ciano <- 3
predict_ciano[predict_ciano$.pred > lim_ciano] <- NA

# medido vs .pred
predict(
  extract_fit_parsnip(mod_lm),
  rr
) |>
  bind_cols(select(rr, ciano)) |>
  ggplot(aes(ciano, .pred)) +
  geom_abline() +
  geom_point(size = 3) +
  coord_equal(xlim = c(1, 5), ylim = c(1, 5)) +
  theme_bw(base_size = 5)

# R2
predict(
  extract_fit_parsnip(mod_lm),
  rr
) |>
  bind_cols(select(rr, ciano)) |>
  rsq(truth = ciano, estimate = .pred)

# RMSE
predict(
  extract_fit_parsnip(mod_lm),
  rr
) |>
  bind_cols(select(rr, ciano)) |>
  rmse(truth = ciano, estimate = .pred)

# mapa -------------------------------------------------------------------

fecha_ciano <- ymd(sub(".rds", "", basename(r_actual)))

etq_ciano <- paste0(
  "Producto HSL\nFecha: ",
  ymd(fecha_ciano),
  "\nReflectancia de superficie"
)

mapa_ciano <- ggplot() +
  # geom_spatraster(data = r$nir, show.legend = FALSE, interpolate = FALSE) +
  # scale_fill_gradient(low = "grey50", high = "grey90") +
  # ggnewscale::new_scale_fill() +
  geom_spatraster(data = predict_ciano, interpolate = FALSE) +
  geom_segment(
    data = flechas_tbl,
    aes(x = xi, y = yi, xend = xf, yend = yf, group = id),
    arrow = arrow(angle = 10, length = unit(2, "mm"), type = "closed"),
    linewidth = .2
  ) +
  # geom_spatvector(
  #   data = emb,
  #   fill = NA,
  #   color = "grey30",
  #   linewidth = .2
  # ) +
  annotate(
    geom = "text",
    x = I(.99),
    y = I(.01),
    label = etq_ciano,
    size = 3,
    family = "Arial",
    hjust = 1,
    vjust = 0,
    lineheight = .8
  ) +
  scale_fill_princess_c(palette = "maori") +
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
  labs(
    fill = "Concentración de clorofila-a<br>de cianobacterias (mg/m<sup>3</sup>)"
  ) +
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
  plot = mapa_ciano,
  filename = "mapa_ciano",
  ancho = 5,
  alto = 5,
  formato = ".tif"
)

# browseURL(paste0(getwd(), "/fig/2025-10/mapa_ciano_2025_10.png"))

extract_ciano <- terra::extract(predict_ciano, v) |>
  as_tibble() |>
  rename(punto = ID)

r2_ciano <- filter(d, fecha == max(d$fecha) & param == "cla_ciano") |>
  select(punto, valor) |>
  inner_join(extract_ciano, by = join_by(punto)) |>
  lm(valor ~ .pred, data = _) |>
  broom::glance() |>
  pull(r.squared)
