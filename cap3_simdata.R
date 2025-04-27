devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_data_tmax.rds") |>
  filter(!missing)


############################
#####   Exploratório   #####
############################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$station.id |> unique() |> length()
#rows.by.group = dplyr::filter(df, !missing) |> select(station.id) |> purrr::pluck(1) |> unique() |> length()

simdata1       = list()
simdata1$info  = "Dados simulados com mesmo tamanho que dados reais de temp. max.; modelo exploratório"
simdata1$model =
  generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols
  )
simdata1$fit     = fastan::run_stan(simdata1$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata1$summary = fastan::summary_matrix(simdata1$fit, simdata1$model)

c = 2
simdata1$summary$lambda[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")] = c * simdata1$summary$lambda[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]
simdata1$summary$alpha[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")]  = (1/c) * simdata1$summary$alpha[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]
fastan::percentage_hits(simdata1$summary)

saveRDS(simdata1, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_simdata_exploratorio.rds")


#############################
#####   Confirmatório   #####
#############################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$regiao |> table() |> unname() |> {\(.) ./cols}()

simdata2       = list()
simdata2$info  = "Dados simulados com mesmo tamanho que dados reais de temp. max.; modelo confirmatório"
simdata2$model = 
  generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols
    )
simdata2$fit     = fastan::run_stan(simdata2$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata2$summary = fastan::summary_matrix(simdata2$fit, simdata2$model)

c_ = c(2.58, 2.48, 2.75, 2.4, 2.62)
for (i in 1:length(c_)) {
  c = c_[i]
  simdata2$summary$lambda[i,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")] = c * simdata2$summary$lambda[i,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]
  simdata2$summary$alpha[,i,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")]  = (1/c) * simdata2$summary$alpha[,i,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]
  
}

fastan::percentage_hits(simdata2$summary)

saveRDS(simdata2, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_simdata_confirmatorio.rds")


##################################
#####   Semi-confirmatório   #####
##################################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$alt_tipo |> table() |> unname() |> {\(.) ./cols}()

simdata3       = list()
simdata3$info  = "Dados simulados com mesmo tamanho que dados reais de temp. max.; modelo semi-confirmatório"
simdata3$model =
  generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols,
    semi.conf = T
    )
simdata3$fit     = fastan::run_stan(simdata3$model, iter = 10000, warmup = 2000, chains = 1, seed = 12345)
simdata3$summary = fastan::summary_matrix(simdata3$fit, simdata3$model)

c = 3.25
simdata3$summary$lambda[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")] = c * simdata3$summary$lambda[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]
simdata3$summary$alpha[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")]  = (1/c) * simdata3$summary$alpha[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]
fastan::percentage_hits(simdata3$summary)

saveRDS(simdata3, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_simdata_semi_confirmatorio.rds")
