devtools::load_all("D:/carlos/01_pesquisa/fastan")
devtools::load_all("D:/carlos/01_pesquisa/meteobr")
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")
library(tidyverse)

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/data_tmax.rds") %>%
  arrange(alt_tipo, station.id, semana) %>%
  #dplyr::filter(!missing) %>%
  #dplyr::filter(station.id %in% sample(df$station.id, 50)) %>%
  filter_by_max_missing("temp_max", "station.id", 40/100)

proj = 
  blank_project() |>
  set_info("dados de temperatura max semanal") |>
  set_data(data = df, value = "temp_max", row  = "station.id", col  = "semana", group = "regiao") |>
  set_space(df = stations, label = "station.id", lat = "lat", lon = "lon", alt = "alt", position = "row") |>
  set_prior(type = "normal", semi.conf = F)

neib_vor = neib_voronoi(proj$space)
neib_dist = neib_dist(proj$space)
car_vor = car(neib_vor)
car_dist = car(neib_dist)

df = proj$space

df$cov_vor = rowSums(car_vor)
df$cov_dist = rowSums(car_dist)

df$cov_vor = rowSums(car_vor - diag(diag(car_vor)))
df$cov_vor = rowSums(car_dist - diag(diag(car_dist)))

df$cov_vor = diag(car_vor)
df$cov_dist = diag(car_dist)

p = list()

p[[1]] =
  plot_map(proj) +
  geom_point(data = df, aes(x = .data$lon, y = .data$lat,
                            color = .data$cov_dist), size = 1.5) +
  scale_color_viridis_c(option = "turbo") +
  labs(title = "dist")

p[[2]] =
  plot_map(proj) +
  geom_point(data = df, aes(x = .data$lon, y = .data$lat,
                            color = .data$cov_vor), size = 1.5) +
  scale_color_viridis_c(option = "turbo") +
  labs(title = "voronoi")

gridExtra::grid.arrange(grobs=p, nrow=1)


plot(rowSums(neib_vor), diag(car_vor))
plot(rowSums(neib_vor), rowSums(car_vor))


par(mfrow=1:2)
plot(rowSums(neib_vor), diag(car_vor))
plot(rowSums(neib_dist), diag(car_dist))
par(mfrow=c(1,1))

