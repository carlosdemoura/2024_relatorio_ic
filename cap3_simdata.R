devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap4_data_tmax.rds") |>
  filter(!missing)


############################
#####   Exploratório   #####
############################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$station.id |> unique() |> length()
#rows.by.group = dplyr::filter(df, !missing) |> select(station.id) |> purrr::pluck(1) |> unique() |> length()

simdata_expl       = list()
simdata_expl$info  = "Dados simulados com mesmo tamanho que dados reais de temp. max.; modelo exploratório"
simdata_expl$model =
  generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols
  )
simdata_expl$fit     = fastan::run_stan(simdata_expl$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata_expl$summary = fastan::summary_matrix(simdata_expl$fit, simdata_expl$model)
fastan::percentage_hits(simdata_expl$summary)

saveRDS(simdata_expl, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_exploratorio.rds")


#############################
#####   Confirmatório   #####
#############################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$regiao |> table() |> unname() |> {\(.) ./cols}()

simdata_conf       = list()
simdata_conf$info  = "Dados simulados com mesmo tamanho que dados reais de temp. max.; modelo confirmatório"
simdata_conf$model = 
  generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols
    )
simdata_conf$fit     = fastan::run_stan(simdata_conf$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata_conf$summary = fastan::summary_matrix(simdata_conf$fit, simdata_conf$model)
fastan::percentage_hits(simdata_conf$summary)

saveRDS(simdata_conf, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_confirmatorio.rds")


##################################
#####   Semi-confirmatório   #####
##################################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$alt_tipo |> table() |> unname() |> {\(.) ./cols}()

simdata_sc       = list()
simdata_sc$info  = "Dados simulados com mesmo tamanho que dados reais de temp. max.; modelo semi-confirmatório"
simdata_sc$model =
  generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols,
    semi.conf = T
    )
simdata_sc$fit     = fastan::run_stan(simdata_sc$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata_sc$summary = fastan::summary_matrix(simdata_sc$fit, simdata_sc$model)
fastan::percentage_hits(simdata_sc$summary)

saveRDS(simdata_sc, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap3_simdata_semi_confirmatorio.rds")
