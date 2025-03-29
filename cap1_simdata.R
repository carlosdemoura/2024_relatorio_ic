####################################
#####   Modelo confirmatório   #####
####################################

set.seed(123456789)

proj1.2       = list()
proj1.2$info  = "Dados simulados para relatório de IC, modelo confirmatório"
proj1.2$model = fastan::generate_data_sc(
  rows.by.group = rep(20, 3),
  columns = 10,
  semi.conf = F
  )
proj1.2$fit   = fastan::run_stan(proj1.2$model, iter = 2000)
proj1.2$summary = fastan::summary_matrix(proj1.2$fit)

saveRDS(proj1.2, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/data/simdata_cap1_conf.rds")


#########################################
#####   Modelo semi-confirmatório   #####
#########################################

set.seed(123456789)

proj1.3       = list()
proj1.3$info  = "Dados simulados para relatório de IC, modelo confirmatório"
proj1.3$model = fastan::generate_data_sc(
  rows.by.group = rep(20, 3),
  columns = 10,
  semi.conf = T
  )
proj1.3$fit   = fastan::run_stan(proj1.3$model, iter = 2000)
proj1.3$summary = fastan::summary_matrix(proj1.3$fit)

saveRDS(proj1.3, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/data/simdata_cap1_semiconf.rds")
