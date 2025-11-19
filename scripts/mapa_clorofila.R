# paquetes ---------------------------------------------------------------

library(tidymodels)
library(ggspatial)
library(tidyterra)

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
  mutate(ratio1 = nir / red, ratio2 = swir2 / green) |>
  select(cla, ratio1, ratio2)

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

predict_cla <- terra::as.data.frame(r_agua$nir, xy = TRUE) |>
  as_tibble() |>
  bind_cols(pred_tbl) |>
  select(-nir) |>
  mutate(.pred = if_else(.pred <= 0, 0, .pred)) |>
  rast()

# histograma
ggplot() +
  geom_histogram(
    data = as.data.frame(predict_cla),
    aes(x = .pred),
    binwidth = 1
  ) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  coord_cartesian(xlim = c(0, 20)) +
  theme_bw(base_size = 4)

# remuevo valores extremos
lim_clorofila <- 10
predict_cla[predict_cla$.pred > lim_clorofila] <- NA

# medido vs .pred
predict(
  extract_fit_parsnip(mod_lm),
  rr
) |>
  bind_cols(select(rr, cla)) |>
  ggplot(aes(cla, .pred)) +
  geom_abline() +
  geom_point(size = 3) +
  coord_equal(xlim = c(1, 5), ylim = c(1, 5)) +
  theme_bw(base_size = 5)

# R2
predict(
  extract_fit_parsnip(mod_lm),
  rr
) |>
  bind_cols(select(rr, cla)) |>
  rsq(truth = cla, estimate = .pred)

# RMSE
predict(
  extract_fit_parsnip(mod_lm),
  rr
) |>
  bind_cols(select(rr, cla)) |>
  rmse(truth = cla, estimate = .pred)

# mapa -------------------------------------------------------------------

fecha_clorofila <- ymd(sub(".rds", "", basename(r_actual)))

etq_clorofila <- paste0(
  "Producto HSL\nFecha: ",
  ymd(fecha_clorofila),
  "\nReflectancia de superficie"
)

mapa_clorofila <- ggplot() +
  geom_spatraster(data = r$nir, show.legend = FALSE, interpolate = FALSE) +
  scale_fill_gradient(low = "grey50", high = "grey90") +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = predict_cla, interpolate = FALSE) +
  geom_segment(
    data = flechas_tbl,
    aes(x = xi, y = yi, xend = xf, yend = yf, group = id),
    arrow = arrow(angle = 10, length = unit(2, "mm"), type = "closed"),
    linewidth = .2
  ) +
  geom_spatvector(
    data = emb,
    fill = NA,
    color = "grey30",
    linewidth = .2
  ) +
  annotate(
    geom = "text",
    x = I(.99),
    y = I(.01),
    label = etq_clorofila,
    size = 3,
    family = "Times New Roman",
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
    text_family = "Times New Roman"
  ) +
  coord_sf(expand = FALSE) +
  labs(fill = "Concentración de<br>clorofila-a (mg/m<sup>3</sup>)") +
  theme_void(base_size = 8, base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
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
  alto = 5
)

# browseURL(paste0(getwd(), "/fig/2025-10/mapa_clorofila_2025_10.png"))
