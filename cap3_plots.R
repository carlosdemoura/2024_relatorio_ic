devtools::load_all("D:/carlos/01_pesquisa/fastan")
devtools::load_all("D:/carlos/01_pesquisa/meteobr")
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plotly)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_data_tmax.rds") %>%
  arrange(alt_tipo, station.id, semana)

##########################
#####   tratamento   #####
##########################

p_missing_max = .2

stations_missings = 
  df %>%
  filter(missing) %>% 
  mutate(
    missing = is.na(temp_max) |> as.numeric()
  ) %>%
  group_by(station.id) %>%
  summarise(missing_p = mean(missing),
            .groups = "drop")

ellegilbe_stations = 
  stations_missings %>%
  filter(missing_p <= p_missing_max) %>%
  select(station.id) %>%
  c() %>%
  purrr::pluck(1)

df =
  df %>%
  filter(!missing | station.id %in% ellegilbe_stations) %>%
  arrange(alt_tipo)

tmax_pred = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_tmax_pred.rds")


##########################
#####   descritiva   #####
##########################

filter(stations, station.id %in% ellegilbe_stations) %>% select(region) %>% table() /71

ggplot(filter(stations, station.id %in% ellegilbe_stations), aes(y=alt, x=region)) +
  geom_boxplot() +
  labs(x = "Região", 
       y = "Altitude (metros)",
       title = "Boxplot das altitudes por região\n(estações com missings)") +
  theme_classic() +
  theme(text=element_text(size=15))
ggsave(img("cap3_geral_boxplot.png"), width = 6, height = 5, dpi = 300, bg = "white")

fastan::plot_missing(tmax_pred$model)
ggsave(img("cap3_geral_contrast.png"), width = 6, height = 5, dpi = 300, bg = "white")

############################################
#####   posteriori preditiva simdata   #####
############################################
station.number = 213

r = 
  tmax_pred$model$pred %>%
  with(which(row == station.number))
tmax_pred$model$pred[r,]
tmax_pred$model$labels$loading[station.number]

tmax_pred$summary$pred |> matrix_to_df()

library(gridExtra)

rows = 5:6

plots = list()
for (i in 1:length(rows)){
  df = simdata$model$pred[rows[i],]
  plots[[i]] =
    plot_posterior(simdata$fit, "pred", row = rows[i]) +
    labs(title = paste0(ellegilbe_stations[df$row], ", sem. ", df$col), x = "") +
    geom_vline(xintercept = simdata$summary$pred[i,1,"real"])
}

grid.arrange(grobs = plots, ncol=3)


##########################################
#####   posteriori preditiva reais   #####
##########################################


which(proj$model$labels$loading == "A537")

xxx %>% filter(row == 9, missing == 1)

rows = 67
plots = list()
for (i in 1:length(rows)){
  df = proj$model$pred[rows[i],]
  plots[[i]] =
    plot_posterior(simdata$fit, "pred", row = rows[i]) +
    labs(title = paste0(proj$model$labels$loading[df$row], ", sem. ", df$col), x = "")
}
grid.arrange(grobs = plots, ncol=3)
