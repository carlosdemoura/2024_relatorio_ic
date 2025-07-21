devtools::load_all("D:/carlos/01_pesquisa/fastan")
source("utils.R")

set_summary2 = function(proj) {
  x = proj
  x$data$real = NULL
  proj$summary = summary_matrix(x)
  proj
}

cap3_espec = list()


###################################################
#####   Dados gerados a partir de um fator,   #####
#####   mas um modelo confirmatório com       #####
#####   dois fatores foi ajustado             #####
###################################################

cap3_espec[["1p2conf"]] =
  new_project() |>
  set_info("estudo de ma especificacao; dados gerados a partir de um fator; modelo conf ajustado com 2 fatores.") |>
  set_data(simdata = T, seed = 12345, group.sizes = 100, columns = 30, semi.conf = F) |>
  {\(.) {
    .$data$obs$group[(nrow(.$data$obs)/2+1):nrow(.$data$obs)] = 2;
    .$data$label$group = factor(c("group 1", "group 2"));
    .$data$dim$group.sizes = c(50, 50);
    .}}() |>
  set_prior(type = "normal", semi.conf = F) |>
  set_fit(iter = 3000, seed = 12345, set_summary = F) |>
  set_summary2()


#######################################################
#####   Dados gerados a partir de dois fatores,   #####
#####   mas um modelo exploratório com um         #####
#####   fator foi ajustado                        #####
#######################################################

cap3_espec[["2p1"]] =
  new_project() |>
  set_info("estudo de ma especificacao; dados gerados a partir de 2 fatores; modelo expl ajustado com 1 fator.") |>
  set_data(simdata = T, seed = 12345, group.sizes = c(50, 50), columns = 30, semi.conf = F) |>
  {\(.) {
    .$data$obs$group = 1;
    .$data$label$group = factor("group 1");
    .$data$dim$group.sizes = 100;
    .}}() |>
  set_prior(type = "normal", semi.conf = F) |>
  set_fit(iter = 3000, seed = 12345, set_summary = F) |>
  set_summary2()


###################################################
#####   Dados gerados a partir de um fator,   #####
#####   mas um modelo semi confirmatório      #####
#####   com dois fatores foi ajustado         #####
###################################################

cap3_espec[["1p2sc"]] =
  new_project() |>
  set_info("estudo de ma especificacao; dados gerados a partir de um fator; modelo semi conf ajustado com 2 fatores.") |>
  set_data(simdata = T, seed = 12345, group.sizes = c(50, 50), columns = 30, semi.conf = F) |>
  {\(.) {
    .$data$obs$group = 1;
    .$data$label$group = factor(c("group 1", "group 2", "group extra"));
    .$data$dim$group.sizes = 100;
    .}}() |>
  set_prior(type = "normal", semi.conf = F) |>
  set_fit(iter = 3000, seed = 12345, set_summary = F) |>
  set_summary2()


########################################
#####   Dados gerados com grande   #####
#####   dispersão das cargas       #####
########################################

cap3_espec[["alphas_grandes"]] =
  new_project() |>
  set_info("estudo de ma especificacao; dados gerados com alphas muito grandes (fora da regao de alta densidade da priori).") |>
  set_data(simdata = T, seed = 12345, group.sizes = rep(30, 5), columns = 30, semi.conf = F,
           dist = dist_uniform(10, 20)) |>
  set_prior(type = "normal", semi.conf = F) |>
  set_fit(iter = 3000, seed = 12345, set_summary = F) |>
  set_summary2()


#####################
#####   Geral   #####
#####################

saveRDS(cap3_espec, "cap3_espec.rds")
saveRDS(lapply(cap3_espec, function (x) {x$fit = NULL; x}), "light/cap3_espec.rds")
