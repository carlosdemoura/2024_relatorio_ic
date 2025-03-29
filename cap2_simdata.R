devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/temp_max_data.rds") |>
  filter(!missing)


############################
#####   Exploratório   #####
############################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$station.id |> unique() |> length()

simdata_expl = 


simdata1       = list()
simdata1$info  = "Dados simulados para relatório de IC, modelo exploratório, mesmo tamanho que dados reais"
simdata1$model =
  fastan::generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols
  )
simdata1$fit     = fastan::run_stan(simdata1$model, iter = 8000)
simdata1$summary = fastan::summary_matrix(simdata1$fit)

saveRDS(simdata1, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/simdata_cap2_exploratorio.rds")


#############################
#####   Confirmatório   #####
#############################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$regiao |> table() |> unname() |> {\(.) ./cols}()

simdata_conf = 
  fastan::generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols
  )

simdata2       = list()
simdata2$info  = "Dados simulados para relatório de IC, modelo confirmatório, mesmo tamanho que dados reais"
simdata2$model = fastan::generate_data_sc(
  rows.by.group = rep(20, 3),
  columns = 10,
  semi.conf = F
)
simdata2$fit     = fastan::run_stan(simdata2$model, iter = 8000)
simdata2$summary = fastan::summary_matrix(simdata2$fit)

saveRDS(simdata2, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/simdata_cap2_confirmatorio.rds")


##################################
#####   Semi-confirmatório   #####
##################################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$alt_tipo |> table() |> unname() |> {\(.) ./cols}()

simdata_conf = 
  fastan::generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols,
    semi.conf = T
  )

simdata3       = list()
simdata3$info  = "Dados simulados para relatório de IC, modelo confirmatório, mesmo tamanho que dados reais"
simdata3$model = fastan::generate_data_sc(
  rows.by.group = rep(20, 3),
  columns = 10,
  semi.conf = F
)
simdata3$fit     = fastan::run_stan(simdata3$model, iter = 8000)
simdata3$summary = fastan::summary_matrix(simdata3$fit)

saveRDS(simdata3, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/simdata_cap2_semi_confirmatorio.rds")