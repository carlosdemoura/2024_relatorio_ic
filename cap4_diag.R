devtools::load_all("D:/carlos/01_pesquisa/meteobr")
devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
library(maps)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")


############################
#####   Exploratório   #####
############################

tmax_expl = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_tmax_exploratorio.rds")
gew_expl = get_gewekes(tmax_expl$fit)
get_gewekes(tmax_expl$fit) |> summary()
get_gewekes(tmax_expl$fit) |> quantile(c(.05,.95))



#############################
#####   Confirmatório   #####
#############################

tmax_conf = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_tmax_confirmatorio.rds")
gew_conf = get_gewekes(tmax_conf$fit)
get_gewekes(tmax_conf$fit) |> summary()
get_gewekes(tmax_conf$fit) |> quantile(c(.05,.95))



##################################
#####   Semi-confirmatório   #####
##################################

tmax_sc = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_tmax_semi_confirmatorio.rds")
gew_sc = get_gewekes(tmax_sc$fit)
get_gewekes(tmax_sc$fit) |> summary()
get_gewekes(tmax_sc$fit) |> quantile(c(.05,.95))




