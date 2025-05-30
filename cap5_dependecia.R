devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

p_missing_max = 50

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_data_tmax.rds") %>%
  arrange(alt_tipo, station.id, semana) %>%
  filter_by_max_missing("temp_max", "station.id", p_missing_max/100)


#######################
#####   simdata   #####
#######################

set.seed(12345)
simdata         = list()
simdata$info    = "Dados simulados com mesmo tamanho que dados reais; modelo semi-confrmatório; com predição; dados faltantes nas mesmas posições que os faltantes no banco real"
simdata$model   = fastan::model_data_sc(df, "temp_max", "alt_tipo", "station.id", "semana", T) |> fiat_simdata_w_pred_from_real_data()
simdata$fit     = run_stan3(simdata$model, iter = 5000, warmup = 2000, chains = 1, seed = 12345)
simdata$summary = fastan::summary_matrix(simdata$fit, simdata$model)
percentage_hits_pred(simdata$summary, simdata$model$real_missing)

saveRDS(simdata, paste0("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap5333_simdata_pred_depend_p", p_missing_max,".rds"))


####################
#####   real   #####
####################

set.seed(12345)
tmax         = list()
tmax$info    = "Dados de temperatura max semanal com dados faltantes; modelo semi-confrmatório por tipo de altitude; com predição"
tmax$model   = fastan::model_data_sc(df, "temp_max", "alt_tipo", "station.id", "semana", T)
tmax$model$pred = NULL
tmax$fit     = run_stan3(tmax$model, iter = 5000, warmup = 1000, seed = 12345)
tmax$summary = fastan::summary_matrix(tmax$fit)

saveRDS(tmax, paste0("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap5333_tmax_pred_p", p_missing_max,".rds"))
