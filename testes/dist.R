devtools::load_all("D:/carlos/01_pesquisa/fastan")
devtools::load_all("D:/carlos/01_pesquisa/meteobr")

source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")
library(tidyverse)

p_missing_max = 40

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/data_tmax.rds") %>%
  arrange(alt_tipo, station.id, semana) %>%
  filter_by_max_missing("temp_max", "station.id", p_missing_max/100)

library(spdep)
library(geosphere)
library(matrixcalc)


proj = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/old_rds/cap4_tmax_confirmatorio.rds")
proj = cap4_tmax_conf
proj = cap4_tmax_sc



coor = 
  stations |>
  filter(station.id %in% unique(df$station.id)) |>
  select(station.id, lat, lon) |>
  {\(.) dplyr::right_join(data.frame(station.id = proj$model$labels$loading), .)}() |>
  rename(lat = "lat", long = "lon") |>
  relocate(long) |>
  as.data.frame() |>
  {\(.) `row.names<-`(., .$station.id)}() |>
  select(!station.id) |>
  as.matrix()


mat =
  coor |>
  spdep::dnearneigh(0, 100, longlat = TRUE) |>
  spdep::nb2mat(style = "B", zero.policy = TRUE) |>
  as.matrix() |>
  unclass() |>
  { \(.) `attributes<-`(., NULL) |> matrix(nrow = nrow(.)) }()

n = nrow(mat)

# número de estações com vizinhos
sum(rowSums(mat) > 0)

# gráfico do número de vizinhos por estação
hist(rowSums(mat), breaks = 0:20)

summary(rowSums(mat))

#mat = neib_mat(coor, rule); mat = mat - diag(rep(mat[1,1], nrow(mat)))
#mat = as.matrix(coor[, c("long", "lat")]) |> distm() |> {\(.) ./1000}(); mat[mat > 200] = 0; mat = mat^-1; mat[is.infinite(mat)] = 0

lapply(proj$prior$alpha$cov, function(d) { is.positive.definite(d + mat*1000) })
lapply(proj$prior$alpha$cov, function(x) { eigen(x + mat*100)$values })
lapply(proj$prior$alpha$cov, function(x) { isSymmetric(x + mat*100) })

proj$prior$alpha$cov = lapply(proj$prior$alpha$cov, function(x) { x + mat })

proj = 
  proj |>
  set_data(simdata = T) |>
  set_fit(iter = 5000, chain = 1)

save_proj(proj)
saveRDS(proj, "precaucao.rds")


distm(coords) / 1000


  
  
inicio = Sys.time()
for (i in seq(37, 100, .3)) {
  x = 
    sapply(proj$prior$alpha$cov, function(x) { is.positive.definite(x + mat*i) }) |>
    #sapply(proj$prior$alpha$cov, function(x) { all(eigen(x + mat*i)$values < 1e-2) }) |>
    any()
  
  if (x) {print(i)}
}
Sys.time() - inicio






diag = rep(10, n) |> diag()
diag = rep(.1, floor(n/2)) |> c(rep(10, ceiling(n/2))) |> diag()

c = .5
is.positive.definite(mat*c + diag)
det(mat*c + diag)
eigen(mat*c + diag)$values
#View(mat*c + diag)

for (c in seq(0.5, 1.5, .01)) {
  if (c == 1){print("METADE")}
  if (is.positive.definite(mat*c + diag)) {
    print(c)
  }
}


dist_mat = as.matrix(coor[, c("long", "lat")]) |> distm() |> {\(.) ./1000}()
dist_vec = dist_mat[lower.tri(dist_mat)]
hist(dist_vec, breaks = seq(1,4500,100))
