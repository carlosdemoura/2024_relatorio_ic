devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
library(ggplot2)
library(reshape2)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_data_tmax.rds") %>%
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


xxx = 
  df %>%
  filter(missing) %>%
  mutate(
    missing = is.na(temp_max) |> as.numeric(),
    col     = semana,
    row     = factor(station.id, levels = unique(station.id)) |> as.numeric()
  )


#######################
#####   simdata   #####
#######################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$alt_tipo |> table() |> unname() |> {\(.) ./cols}()

simdata_mod = 
  generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols,
    semi.conf = T
  )

missings =
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
  
simdata_mod$pred = missings

missings$value = 9999
xxx = left_join(simdata_mod$data, missings, by = c("row", "col", "group"))
simdata_mod$data =
  xxx %>%
  filter(is.na(value.y)) %>%
  select(!value.y) %>%
  rename(value = "value.x")

simdata_mod$pred = NULL

simdata         = list()
simdata$info    = "Dados simulados com mesmo tamanho que dados reais; modelo semi-confrmatório; com predição; dados faltantes nas mesmas posições que os faltantes no banco real"
simdata$model   = simdata_mod
simdata$fit     = fastan::run_stan(simdata$model, iter = 50, warmup = 10, chains = 1, seed = 12345)
simdata$summary = fastan::summary_matrix(simdata$fit, simdata$model)
fastan::percentage_hits(simdata$summary)

saveRDS(simdata, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap5_simdata_ignore.rds")


####################
#####   real   #####
####################

tmax       = list()
tmax$info  = "Dados de temperatura max semanal com dados faltantes; modelo semi-confrmatório por tipo de altitude; com predição"
tmax$model = fastan::model_data_sc(df, "temp_max", "alt_tipo", "station.id", "semana", T)

tmax$model$data =
  tmax$model$data |>
  {\(.) dplyr::filter(., !is.na(.$value))}()

tmax$fit     = fastan::run_stan(tmax$model, iter = 10000, warmup = 4000, seed = 12345)
tmax$summary = fastan::summary_matrix(tmax$fit)

saveRDS(tmax, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap5_tmax_ignore.rds")
