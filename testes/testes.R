devtools::load_all("D:/carlos/01_pesquisa/meteobr")
devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
library(maps)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

theme_map = 
  theme(
    text = element_text(size = 15),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )

br_map = map_data("world", region = "Brazil")

tmax_expl = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_tmax_exploratorio.rds")
tmax_conf = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_tmax_confirmatorio.rds")
tmax_sc = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_tmax_semi_confirmatorio.rds")


alpha = 
  tmax_expl$summary$alpha[,,"mean"] %>%
  data.frame() %>%
  `colnames<-`("alpha") %>%
  mutate(
    station.id = tmax_expl$model$labels$loading
  ) %>%
  merge(stations[c("station.id", "lat", "lon")], by = "station.id")

ggplot() +
  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = alpha, aes(x = lon, y = lat, color = alpha),
             size = 2) +
  scale_color_viridis_c(option = "turbo") +
  labs(
    title = "Loadings das estações",
    color = "Esperança\nposterior\nda carga"
  ) +
  coord_fixed() +
  theme_map



alpha = 
  tmax_sc$summary$alpha[,2,"mean"] %>%
  data.frame() %>%
  `colnames<-`("alpha") %>%
  mutate(
    station.id = tmax_conf$model$labels$loading
  ) %>%
  merge(stations[c("station.id", "lat", "lon")], by = "station.id")

ggplot() +
  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = alpha, aes(x = lon, y = lat, color = alpha),
             size = 2) +
  scale_color_viridis_c(option = "turbo") +
  labs(
    title = "Loadings das estações",
    color = "Esperança\nposterior\nda carga"
  ) +
  coord_fixed() +
  theme_map

