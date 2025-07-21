devtools::load_all("D:/carlos/01_pesquisa/meteobr")
devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
source("utils.R")


#####################
#####   Geral   #####
#####################

df = readRDS("df.rds")

summary(df$temp_max)
table(stations$region)
table(stations$region) / sum(table(stations$region))
summary(stations$alt)

ggplot(stations, aes(y=alt, x=region)) +
  geom_boxplot() +
  labs(x = "Região", 
       y = "Altitude (metros)",
       title = "Boxplot das altitudes por região") +
  theme_classic() +
  text_size(15)
ggsave(img("cap4_geral_boxplot_alt"), width = 6, height = 5, dpi = 300, bg = "white")


############################
#####   Exploratório   #####
############################

cap4_tmax_expl = 
  new_project() |>
  set_info("dados de temperatura max semanal; modelo exploratório.") |>
  set_data(data = df, value = "temp_max", row  = "station.id", col  = "semana") |>
  set_space(df = stations, label = "station.id", lat = "lat", lon = "lon", alt = "alt", position = "row") |>
  set_prior(type = "normal", semi.conf = F,
            engine = list(
              alpha  = function(x) car_expl(x, neib_voronoi, 400),
              lambda = function(x) car_simple(x, 1),
              sigma2 = function(x) list(shape = .1, rate = .1)
            )) |>
  set_fit(iter = 1e5, seed = 12345)
save_proj(cap4_tmax_expl)

#cap4_tmax_expl = readRDS("cap4_tmax_expl.rds")

plot_hpd(cap4_tmax_expl, par = "alpha", col = 1, stat = "mean") + text_size(20)
ggsave(img("cap4_expl_alpha"), width = 15, height = 5, dpi = 300, bg = "white")

plot_hpd(cap4_tmax_expl, par = "lambda", row = 1, stat = "mean") + text_size(15)
ggsave(img("cap4_expl_lambda"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(cap4_tmax_expl, par = "sigma2", col = 1, stat = "mean") + text_size(20)
ggsave(img("cap4_expl_sigma2"), width = 15, height = 5, dpi = 300, bg = "white")


###  Histograma loadings  ###

loadings =
  summary_as_df(cap4_tmax_expl, "alpha")[["alpha"]] %>%
  left_join(cap4_tmax_expl$space, by = "row") %>%
  mutate(
    grupo = ifelse(mean < median(mean), 1, 2)
  )

ggplot(loadings, aes(x = alt, fill = as.factor(grupo))) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 20, 
                 alpha = 0.25, 
                 position = "identity", 
                 color = NA) +
  scale_fill_manual(
    values = c("blue", "red"),
    labels = c("baixa", "alta")
  ) +
  labs(fill = "Esperança do loading",
       x = "Altitude", 
       y = "Frequência",
       title = "Loadings vs altitude") +
  theme_minimal() +
  text_size(15)
ggsave(img("cap4_expl_loading"), width = 8, height = 5, dpi = 300, bg = "white")


###  Histograma variância  ###

variance =
  summary_as_df(cap4_tmax_expl, "sigma2")[["sigma2"]] %>%
  rename(station.id = "id") %>%
  left_join(stations, by = "station.id")

ggplot(variance, aes(x = mean, fill = region)) + 
  geom_histogram(alpha = 0.5,
                 aes(y = after_stat(density))) +
  labs(fill = "Região",
       x = "Média à posteriori da variância", 
       y = "Frequência",
       title = "Variância vs região") +
  theme_minimal() +
  text_size(15)
ggsave(img("cap4_expl_variance"), width = 8, height = 5, dpi = 300, bg = "white")


#############################
#####   Confirmatório   #####
#############################

cap4_tmax_conf = 
  new_project() |>
  set_info("dados de temperatura max semanal; modelo confirmatório; grupos por região.") |>
  set_data(data = df, value = "temp_max", row  = "station.id", col  = "semana", group = "regiao") |>
  set_space(df = stations, label = "station.id", lat = "lat", lon = "lon", alt = "alt", position = "row") |>
  set_prior(type = "normal", semi.conf = F,
            engine = list(
              alpha  = function(x) car_conditional(x, neib_voronoi, 400),
              lambda = function(x) car_simple(x, 1),
              sigma2 = function(x) list(shape = .1, rate = .1)
            )) |>
  set_fit(iter = 1e5, seed = 12345)
save_proj(cap4_tmax_conf)

#cap4_tmax_conf = readRDS("cap4_tmax_conf.rds")

plot_contrast(cap4_tmax_conf) + text_size(20)
ggsave(img("cap4_conf_alpha"), width = 8, height = 8, dpi = 300, bg = "white")

plot_lambda(cap4_tmax_conf) + text_size(15)
ggsave(img("cap4_conf_lambda"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(cap4_tmax_conf, par = "sigma2", col = 1, stat = "mean") + text_size(20)
ggsave(img("cap4_conf_sigma2"), width = 15, height = 5, dpi = 300, bg = "white")

plot_map_data(cap4_tmax_conf, "var") + text_size(15)
ggsave(img("cap4_conf_mapa_var_amostra"), width = 7, height = 5, dpi = 300, bg = "white")

plot_map_post(cap4_tmax_conf, "sigma2", 1, "mean") + text_size(15)
ggsave(img("cap4_conf_mapa_var_post"), width = 7, height = 5, dpi = 300, bg = "white")


##################################
#####   Semi-confirmatório   #####
##################################

cap4_tmax_sc = 
  new_project() |>
  set_info("dados de temperatura max semanal; modelo confirmatório; grupos por altitude (dividos em terços).") |>
  set_data(data = df, value = "temp_max", row  = "station.id", col  = "semana", group = "alt_tipo") |>
  set_space(df = stations, label = "station.id", lat = "lat", lon = "lon", alt = "alt", position = "row") |>
  set_prior(type = "normal", semi.conf = T,
            engine = list(
              alpha  = function(x) car_conditional(x, neib_voronoi, 400),
              lambda = function(x) car_simple(x, 1),
              sigma2 = function(x) list(shape = .1, rate = .1)
            )) |>
  set_fit(iter = 1e5, seed = 12345)
save_proj(cap4_tmax_sc)

#cap4_tmax_sc = readRDS("cap4_tmax_sc.rds")

plot_hpd(cap4_tmax_sc, par = "alpha", col = 1, stat = "mean") + text_size(20)
ggsave(img("cap4_sc_alpha1"), width = 15, height = 5, dpi = 300, bg = "white")

plot_contrast(cap4_tmax_sc, stat = "mean") + text_size(20)
ggsave(img("cap4_sc_contrast1"), width = 8, height = 8, dpi = 300, bg = "white")

plot_contrast(cap4_tmax_sc, stat = "hpd_contains_0") + text_size(20)
ggsave(img("cap4_sc_contrast2"), width = 8, height = 8, dpi = 300, bg = "white")

plot_lambda(cap4_tmax_sc) + text_size(15)
ggsave(img("cap4_sc_lambda"), width = 10, height = 5, dpi = 300, bg = "white")

plot_map_post_factor(cap4_tmax_sc, F, .5) + text_size(15)
ggsave(img("cap4_sc_station_factor"), width = 10, height = 5, dpi = 300, bg = "white")

plot_map_post_factor(cap4_tmax_sc, T, .5) + text_size(15)
ggsave(img("cap4_sc_station_factor_extra"), width = 10, height = 5, dpi = 300, bg = "white")