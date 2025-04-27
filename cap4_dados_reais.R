devtools::load_all("D:/carlos/01_pesquisa/meteobr")
devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)

df = 
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_data_tmax.rds") %>%
  filter(!missing) %>%
  arrange(regiao, station.id, semana)


############################
#####   Exploratório   #####
############################

set.seed(12345)

tmax_expl       = list()
tmax_expl$info  = "dados de temperatura max semanal; modelo exploratório"
tmax_expl$model =
  fastan::model_data_sc(
    df |> mutate(x = "unique"),
    value = "temp_max",
    group = "x",
    row  = "station.id",
    col  = "semana",
    semi.conf = F
  )
tmax_expl$fit     = fastan::run_stan(tmax_expl$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
tmax_expl$summary = fastan::summary_matrix(tmax_expl$fit)

saveRDS(tmax_expl, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_tmax_exploratorio.rds")


#############################
#####   Confirmatório   #####
#############################

set.seed(12345)

tmax_conf       = list()
tmax_conf$info  = "dados de temperatura max semanal; modelo confirmatório; grupos por região"
tmax_conf$model =
  fastan::model_data_sc(
    arrange(df, regiao, station.id, semana),
    value = "temp_max",
    group = "regiao",
    row  = "station.id",
    col  = "semana",
    semi.conf = F
  )
tmax_conf$fit     = fastan::run_stan(tmax_conf$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
tmax_conf$summary = fastan::summary_matrix(tmax_conf$fit)

saveRDS(tmax_conf, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_tmax_confirmatorio.rds")


##################################
#####   Semi-confirmatório   #####
##################################

set.seed(12345)

tmax_sc       = list()
tmax_sc$info  = "dados de temperatura max semanal; modelo confirmatório; grupos por altitude (dividos em terços)"
tmax_sc$model =
  fastan::model_data_sc(
    arrange(df, alt_tipo, station.id, semana),
    value = "temp_max",
    group = "alt_tipo",
    row  = "station.id",
    col  = "semana",
    semi.conf = T
  )
tmax_sc$fit     = fastan::run_stan(tmax_sc$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
tmax_sc$summary = fastan::summary_matrix(tmax_sc$fit)

saveRDS(tmax_sc, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_tmax_semi_confirmatorio.rds")
