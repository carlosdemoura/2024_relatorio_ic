devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
library(ggplot2)
library(reshape2)

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_data_tmax.rds")


##########################
#####   tratamento   #####
##########################

ellegilbe_stations = 
  df %>%
  filter(missing) %>% 
  mutate(
    missing = is.na(temp_max) |> as.numeric()
  ) %>%
  group_by(station.id) %>%
  summarise(missing_p = mean(missing),
            .groups = "drop") %>%
  filter(missing_p < .3) %>%
  select(station.id) %>%
  c() %>%
  purrr::pluck(1)

df =
  df %>%
  filter(!missing | station.id %in% ellegilbe_stations) %>%
  arrange(alt_tipo)

df2 = 
  df %>%
  filter(
    station.id %in% x1
  ) %>% 
  mutate(
    present = is.na(temp_max_week) |> as.numeric(),
    row = factor(station.id) |> as.numeric(),
    col = week
  ) %>%
  select(row, col, present)

ggplot(df2, aes(col, row, fill = factor(present))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "white", "0" = "black")) +
  theme_minimal() +
  labs(x = "Coluna", y = "Linha", fill = "Valor") +
  scale_y_reverse()


#######################
#####   simdata   #####
#######################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$alt_tipo |> table() |> unname() |> {\(.) ./cols}()

simdata_mod = 
  fastan::generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols,
    semi.conf = T
  )

missing =
  fastan::model_data_sc(
    df,
    value = "temp_max",
    group = "alt_tipo",
    row = "station.id",
    col = "semana",
    semi.conf = T
  ) %>%
  .$data %>%
  filter(is.na(value))
  
simdata_mod$pred
simdata_mod$data

simdata$info  = "Dados simulados com mesmo tamanho que dados reais; modelo semi-confrmatório; com predição; dados faltantes nas mesmas posições que os faltantes no banco real"
simdata$model = simdata_mod
simdata$fit     = fastan::run_stan(simdata$model, iter = 8000)
simdata$summary = fastan::summary_matrix(simdata$fit)

saveRDS(simdata, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata.rds")


####################
#####   real   #####
####################

proj = list()
proj$info    = "Dados de temperatura max semanal com dados faltantes; modelo semi-confrmatório por tipo de altitude; com predição"
proj$model   = fastan::model_data_sc(df, "temp_max", "alt_tipo", "station.id", "semana", T)
proj$fit     = fastan::run_stan(proj$model, iter = 8000)
proj$summary = fastan::summary_matrix(simdata$fit)






####   Estações elegíveis

df2 = 
  df %>%
  filter(
    station.id %in% x1
  ) %>% 
  mutate(
    missing = is.na(temp_max_week) |> as.numeric()
  ) %>%
  group_by(station.id) %>%
  summarise(missing_p = mean(missing),
            .groups = "drop")

ellegilbe_stations = filter(df2, missing_p < .5) %>% select(station.id) %>% c()
