devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

################################
#####   Geração de dados   #####
################################

simdata_expl       = list()
simdata_expl$info  = "Dados simulados a partir de um modelo exploratório"
simdata_expl$model = 
  generate_data_sc(
    rows.by.group = 100,
    columns = 50
  )

simdata_conf       = list()
simdata_conf$info  = "Dados simulados a partir de um modelo confirmatório"
simdata_conf$model = 
  generate_data_sc(
    rows.by.group = c(50,50),
    columns = 50
  )

simdata_sc       = list()
simdata_sc$info  = "Dados simulados a partir de um modelo semi-confirmatório"
simdata_sc$model = 
  generate_data_sc(
    rows.by.group = c(33,33,34),
    columns = 50,
    semi.conf = T
  )

models = 
  list(
    "expl" = simdata_expl,
    "conf" = simdata_conf,
    "sc" = simdata_sc
  )
saveRDS(expl_as_conf, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/ap2_models.rds")

################################
#####     Exploratório     #####
#####   as Confirmatório   #####
################################

simdata_expl_as_conf            = list()
simdata_expl_as_conf$info       = "Dados simulados a partir de um modelo confirmatório, mas ajustamos modelo confirmatorio com 2 fatores"
simdata_expl_as_conf$model      = conf_mod
simdata_expl_as_conf$model$real = expl_mod$real
simdata_expl_as_conf$model$data =
  conf_mod$data %>%
  mutate(
    value = expl_mod$data$value
  )

simdata_expl_as_conf$fit     = fastan::run_stan(expl_as_conf$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata_expl_as_conf$summary = fastan::summary_matrix(expl_as_conf$fit)

saveRDS(expl_as_conf, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_expl_as_conf.rds")


##################################
####       Exploratório     ######
####  as Semi-confirmatório  #####
##################################

simdata_expl_as_sc            = list()
simdata_expl_as_sc$info       = "Dados simulados a partir de um modelo confirmatório, mas ajustamos modelo semi-confirmatorio com 2 fatores"
simdata_expl_as_sc$model      = sc_mod
simdata_expl_as_sc$model$real = expl_mod$real
simdata_expl_as_sc$model$data =
  sc_mod$data %>%
  mutate(
    value = expl_mod$data$value
  )

simdata_expl_as_sc$fit     = fastan::run_stan(simdata_expl_as_sc$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata_expl_as_sc$summary = fastan::summary_matrix(simdata_expl_as_sc$fit)

saveRDS(simdata_expl_as_sc, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_expl_as_sc.rds")


################################
#####    Confirmatório     #####
#####   as Exploratório   ######
################################

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
####      Confirmatório     ######
####  as Semi-confirmatório  #####
##################################



###############################
####  Semi-confirmatório  #####
####   as Confirmatório   #####
###############################

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
