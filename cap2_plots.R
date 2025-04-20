devtools::load_all("D:/carlos/01_pesquisa/meteobr")
devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
library(maps)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")


#####################
#####   Geral   #####
#####################

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

ggplot() +
  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = stations, aes(x = lon, y = lat),
             size = 1) +
  labs(title = "Distribuição espacial das estações") +
  coord_fixed() + 
  theme_map
ggsave(img("cap2_geral_mapa_estacoes.png"), width = 5, height = 5, dpi = 300, bg = "white")


df =
  readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_data_tmax.rds") %>%
  filter(!missing)

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
  theme(text=element_text(size=15))
ggsave(img("cap2_geral_boxplot_alt.png"), width = 6, height = 5, dpi = 300, bg = "white")


df2 = 
  stations %>%
  filter(station.id %in% unique(df$station.id))
table(df2$region)
table(df2$region) / sum(table(df2$region))

table(df$alt_tipo)/52


############################
#####   Exploratório   #####
############################

tmax_expl = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_tmax_exploratorio.rds")

plot_hpd(tmax_expl$summary, par = "alpha", col = 1, stat = "mean") + theme(text=element_text(size=20))
ggsave(img("cap2_expl_alpha.png"), width = 15, height = 5, dpi = 300, bg = "white")

plot_hpd(tmax_expl$summary, par = "lambda", row = 1, stat = "mean") + theme(text=element_text(size=15))
ggsave(img("cap2_expl_lambda.png"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(tmax_expl$summary, par = "sigma2", col = 1, stat = c("mean", "median"))  + theme(text=element_text(size=20))
ggsave(img("cap2_expl_sigma2.png"), width = 15, height = 5, dpi = 300, bg = "white")


###  Histograma loadings  ###

loadings =
  tmax_expl$summary$alpha %>%
  matrix_to_df() %>%
  mutate(
    station.id = tmax_expl$model$labels$loading
  ) %>%
  merge(stations, by = "station.id") %>%
  mutate(
    grupo = ifelse(mean < median(mean), 1, 2)
  )

ggplot(loadings, aes(x = alt, fill = as.factor(grupo))) +
  geom_histogram(data = filter(loadings, grupo == 1), 
                 aes(y = after_stat(density)),
                 bins = 20, 
                 alpha = 0.25, 
                 position = "identity", 
                 color = NA) +
  geom_histogram(data = filter(loadings, grupo == 2), 
                 aes(y = after_stat(density)),
                 bins = 20, 
                 alpha = 0.25, 
                 position = "identity", 
                 color = NA) +
  scale_fill_manual(
    values = c("blue", "red"),
    labels = c("baixa", "alta")  # Personalizando os rótulos
  ) +
  labs(fill = "Esperança do loading",  # Título da legenda
       x = "Altitude", 
       y = "Frequência",
       title = "Loadings vs altitude") +
  theme_minimal() +
  theme(text=element_text(size=15))
ggsave(img("cap2_expl_loading.png"), width = 8, height = 5, dpi = 300, bg = "white")


###  Histograma variância  ###

variance = tmax_expl$summary$sigma2 |> matrix_to_df()
variance$station.id = tmax_expl$model$labels$loading
variance = merge(x = variance, y = stations, by = "station.id")

ggplot(variance, aes(x = mean, fill = region)) + 
  geom_histogram(alpha = 0.5,
                 aes(y = after_stat(density))) +
  labs(fill = "Região",
       x = "Média à posteriori da variância", 
       y = "Frequência",
       title = "Variância vs região") +
  theme_minimal() +
  theme(text=element_text(size=15))
ggsave(img("cap2_expl_variance.png"), width = 8, height = 5, dpi = 300, bg = "white")


#############################
#####   Confirmatório   #####
#############################

tmax_conf = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_tmax_confirmatorio.rds")

plot_contrast(tmax_conf$summary) + theme(text=element_text(size=20))
ggsave(img("cap2_conf_alpha.png"), width = 8, height = 8, dpi = 300, bg = "white")

plot_lambda(tmax_conf$summary) + theme(text=element_text(size=15))
ggsave(img("cap2_conf_lambda.png"), width = 10, height = 5, dpi = 300, bg = "white")

plot_hpd(tmax_conf$summary, par = "sigma2", col = 1, stat = c("mean", "median"))  + theme(text=element_text(size=20))
ggsave(img("cap2_conf_sigma2.png"), width = 15, height = 5, dpi = 300, bg = "white")


###  Mapa das variâncias amostrais  ###

mvar = 
  tmax_conf$model$data %>%
  group_by(row) %>% 
  summarise(var = var(value),
            .groups = 'drop') %>%
  mutate(
    station.id = tmax_conf$model$labels$loading
  ) %>%
  merge(stations, by = "station.id")

ggplot() +
  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = mvar, aes(x = lon, y = lat, color = var),
             size = 2) +
  scale_color_viridis_c(option = "turbo") +
  labs(
    title = "Estações por variância amostral no ano",
    color = "Variância\namostral"
  ) +
  coord_fixed() +
  theme_map
ggsave(img("cap2_conf_mapa_var_amostra.png"), width = 7, height = 5, dpi = 300, bg = "white")


###  Mapa das variâncias a posteriori  ###

sigma2 = 
  tmax_conf$summary$sigma2 %>%
  matrix_to_df() %>%
  mutate(
    station.id = tmax_conf$model$labels$loading
  ) %>%
  merge(stations, by = "station.id")

ggplot() +
  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = sigma2, aes(x = lon, y = lat, color = mean),
             size = 2) +
  scale_color_viridis_c(option = "turbo") +
  labs(
    title = "Estações por média da variância a posteriori",
    color = "Média a\nposteriori\nda variância"
  ) +
  coord_fixed() +
  theme_map
ggsave(img("cap2_conf_mapa_var_post.png"), width = 7, height = 5, dpi = 300, bg = "white")


##################################
#####   Semi-confirmatório   #####
##################################

tmax_sc = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_tmax_semi_confirmatorio.rds")

plot_hpd(tmax_sc$summary, par = "alpha", col = 1, stat = "mean") + theme(text=element_text(size=20))
ggsave(img("cap2_sc_alpha1.png"), width = 15, height = 5, dpi = 300, bg = "white")

plot_contrast(tmax_sc$summary, stat = "mean") + theme(text=element_text(size=20))
ggsave(img("cap2_sc_contrast1.png"), width = 8, height = 8, dpi = 300, bg = "white")

plot_contrast(tmax_sc$summary, stat = "hpd_contains_0") + theme(text=element_text(size=20))
ggsave(img("cap2_sc_contrast2.png"), width = 8, height = 8, dpi = 300, bg = "white")

plot_lambda(tmax_sc$summary) + theme(text=element_text(size=15))
ggsave(img("cap2_sc_lambda.png"), width = 10, height = 5, dpi = 300, bg = "white")


###  Mapa grupo extra  ###

alpha =
  data.frame(tmax_sc$summary$alpha[,,"hpd_contains_0"]) %>%
  `colnames<-`(c("col1", "col2")) %>%
  mutate(
    station.id = tmax_sc$model$labels$loading,
    extra = F
  ) %>%
  as_tibble()

alpha[274:nrow(alpha),"extra"] = T

alpha = 
  alpha %>%
  mutate(
    fator = 
      case_when(
        (col1 == T) & (col2 == F) ~ "baixa",
        (col1 == F) & (col2 == T) ~ "alta",
        (col1 == F) & (col2 == F) ~ "ambos",
        TRUE ~ "nenhum"
  )) %>%
  merge(stations, by = "station.id")


ggplot() +
  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = filter(alpha, extra == T), aes(x = lon, y = lat, color = fator), size = 2) +
  scale_color_manual(values = c("red", "blue", "green4", "pink")) +
  labs(
    title = "Estações do grupo extra por fator",
    color = "Fator"
  ) +
  coord_fixed() +
  theme_map
ggsave(img("cap2_sc_mapa_extra.png"), width = 6, height = 5, dpi = 300, bg = "white")


###  Mapa todos os grupos  ###

ggplot() +
  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = alpha, aes(x = lon, y = lat, color = fator), size = 2) +
  scale_color_manual(values = c("red", "blue", "green4", "pink")) +
  labs(
    title = "Estações por fator",
    color = "Fator"
  ) +
  coord_fixed() +
  theme_map
ggsave(img("cap2_sc_mapa_geral.png"), width = 6, height = 5, dpi = 300, bg = "white")
