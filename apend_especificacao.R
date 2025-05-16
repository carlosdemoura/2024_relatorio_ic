devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_data_tmax.rds") |>
  filter(!missing)

expl_mod = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_exploratorio.rds")$model
conf_mod = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_confirmatorio.rds")$model
sc_mod   = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_semi_confirmatorio.rds")$model

############################
#####   Exploratório   #####
############################

###   confirmatório   ###

expl_as_conf         = list()
expl_as_conf$info    = "Dados simulados com mesmo tamanho que dados reais de temp. max.; dados gerados a partir de modelo exploratório, mas modelo confirmatorio ajustado"

expl_as_conf$model      = conf_mod
expl_as_conf$model$real = expl_mod$real
expl_as_conf$model$data =
  conf_mod$data %>%
  mutate(
    value = expl_mod$data$value
  )

expl_as_conf$fit     = fastan::run_stan(expl_as_conf$model, iter = 5000, warmup = 1000, chains = 1, seed = 12345)
expl_as_conf$summary = fastan::summary_matrix(expl_as_conf$fit)

saveRDS(expl_as_conf, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_expl_as_conf.rds")


###   semi-confirmatório   ###

expl_as_sc         = list()
expl_as_sc$info    = "Dados simulados com mesmo tamanho que dados reais de temp. max.; dados gerados a partir de modelo exploratório, mas modelo semi-confirmatorio ajustado"

expl_as_sc$model      = sc_mod
expl_as_sc$model$real = expl_mod$real
expl_as_sc$model$data =
  sc_mod$data %>%
  mutate(
    value = expl_mod$data$value
  )

expl_as_sc$fit     = fastan::run_stan(expl_as_sc$model, iter = 5000, warmup = 1000, chains = 1, seed = 12345)
expl_as_sc$summary = fastan::summary_matrix(expl_as_sc$fit)

saveRDS(expl_as_sc, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_expl_as_sc.rds")


#############################
#####   Confirmatório   #####
#############################

###   exploratório   ###

conf_as_expl         = list()
conf_as_expl$info    = "Dados simulados com mesmo tamanho que dados reais de temp. max.; dados gerados a partir de modelo confirmatório, mas modelo exploratório ajustado"

conf_as_expl$model      = expl_mod
conf_as_expl$model$real = conf_mod$real
conf_as_expl$model$data =
  expl_mod$data %>%
  mutate(
    value = conf_mod$data$value
  )

conf_as_expl$fit     = fastan::run_stan(conf_as_expl$model, iter = 4000, warmup = 500, chains = 1, seed = 12345)
conf_as_expl$summary = fastan::summary_matrix(conf_as_expl$fit)

saveRDS(conf_as_expl, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_conf_as_expl.rds")


##################################
#####   Semi-confirmatório   #####
##################################

###   exploratório   ###

sc_as_expl         = list()
sc_as_expl$info    = "Dados simulados com mesmo tamanho que dados reais de temp. max.; dados gerados a partir de modelo semi-confirmatório, mas modelo exploratório ajustado"

sc_as_expl$model      = expl_mod
sc_as_expl$model$real = sc_mod$real
sc_as_expl$model$data =
  expl_mod$data %>%
  mutate(
    value = sc_mod$data$value
  )

sc_as_expl$fit     = fastan::run_stan(sc_as_expl$model, iter = 4000, warmup = 500, chains = 1, seed = 12345)
sc_as_expl$summary = fastan::summary_matrix(sc_as_expl$fit)

saveRDS(sc_as_expl, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_sc_as_expl.rds")



