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
