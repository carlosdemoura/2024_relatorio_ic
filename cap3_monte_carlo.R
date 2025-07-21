devtools::load_all("D:/carlos/01_pesquisa/fastan")

get_accuracy = function(proj) {
  accu =
    accuracy(proj)[1:4,1:2]
  
  hpd_amp =
    proj$summary |>
    lapply(function(x) mean(x[,,"hpd_amp"])) |>
    unlist()
  
  diag =
    proj$diagnostic |>
    dplyr::group_by_at("par") |>
    summarise(Rhat= max(Rhat), geweke = max(abs(`geweke:1`))) |>
    dplyr::filter(par != "lp__") |>
    {\(.) .[c(1,2,4,3),2:3] }()
  
  cbind(accu, hpd_amp, diag)
}

rodadas = 100
set.seed(12345)
seeds = stats::runif(rodadas, 1e5, 1e6) |> floor()

MC_conf = MC_sc = 
  list(
    accuracy = array(0, dim = c(rodadas, 4, 5), dimnames = list(NULL, c("alpha", "lambda", "sigma2", "pred"), c("p", "bias", "hpd_amp", "Rhat", "geweke"))),
    time     = matrix(0, nrow = rodadas, ncol = 2, dimnames = list(NULL, c("warmup", "sampling")))
  )

proj_conf = proj_sc = list()

for (i in 1:rodadas) {
  set.seed(seeds[i])
  proj_conf[[i]] =
    new_project() |>
    set_data(simdata = T, group.sizes = rep(30, 3), columns = 20, semi.conf = F, pred = .1) |>
    set_space(type = "random", cont = T) |>
    set_prior(type = "normal", semi.conf = F,
              engine = list(
                alpha  = function(x) car_conditional(x, neib_voronoi, 400),
                lambda = function(x) car_simple(x, 1),
                sigma2 = function(x) list(shape = .1, rate = .1)
              )) |>
    set_data(simdata = "prior", seed = 12345) |>
    set_fit(iter = 2000, seed = 12345)
  
  #proj_conf[[i]]$fit = NULL
  
  MC_conf$accuracy[i,,] = get_accuracy(proj)
  MC_conf$time[i,] = rstan::get_elapsed_time(proj$fit) |> c()
}
MC_conf$time =
  MC_conf$time |>
  {\(.) cbind(., "total" = rowSums(.)) }()
saveRDS(MC_conf, "cap3_MC_conf_table.rds")
saveRDS(proj_conf, "cap3_MC_conf_projs.rds")


for (i in 1:rodadas) {
  set.seed(seeds[i])
  proj_sc[[i]] =
    new_project() |>
    set_data(simdata = T, group.sizes = rep(30, 4), columns = 20, semi.conf = T, pred = .1) |>
    set_space(type = "random", cont = F) |>
    set_prior(type = "normal", semi.conf = T,
              engine = list(
                alpha  = function(x) car_conditional(x, neib_voronoi, 400),
                lambda = function(x) car_simple(x, 1),
                sigma2 = function(x) list(shape = .1, rate = .1)
              )) |>
    set_data(simdata = "prior", seed = 12345) |>
    set_fit(iter = 2000, seed = 12345)
  
  #proj_sc[[i]]$fit = NULL
  
  MC_sc$accuracy[i,,] = get_accuracy(proj)
  MC_sc$time[i,] = rstan::get_elapsed_time(proj$fit) |> c()
}
MC_sc$time =
  MC_sc$time |>
  {\(.) cbind(., "total" = rowSums(.)) }()
saveRDS(MC_sc, "cap3_MC_sc_table.rds")
saveRDS(proj_sc, "cap3_MC_sc_projs.rds")
