
devtools::load_all("D:/carlos/01_pesquisa/fastan")
devtools::load_all("D:/carlos/01_pesquisa/meteobr")
source("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/utils.R")
library(tidyverse)

df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/data_tmax.rds") |>
  arrange(alt_tipo, station.id, semana) |>
  filter_by_max_missing("temp_max", "station.id", 50/100)


proj=
  proj |>
  set_fit2(iter = 100, seed = 12345) |>
  set_fit2(iter = 100, seed = 12345) |>
  {\(.) {export(., "C:/Users/Carlos/Downloads"); .}}()




proj1 =
  blank_project() |>
  set_info("dados de temperatura max semanal") |>
  set_data(data = df, value = "temp_max", row  = "station.id", col  = "semana", group = "regiao") |>
  set_space(df = stations, label = "station.id", lat = "lat", lon = "lon", alt = "alt", position = "row") |>
  set_prior(type = "normal", semi.conf = F, alpha0 = T) |>
  {\(.) {.$prior$alpha$cov   = car_conditional(.); . }}() |>
  {\(.) {.$prior$alpha$cov   = .$prior$alpha$cov  |> lapply(function(x) x * 400); . }}() |>
  {\(.) {.$prior$lambda$cov  = .$prior$lambda$cov |> lapply(function(x) car(neib_simple(ncol(x)))) ; . }}() |>
  set_data(simdata = "prior", seed = 12345) |>
  set_fit2(iter = 10000, seed = 12345) |>
  {\(.) {export(., "C:/Users/Carlos/Downloads"); .}}()



proj2 =
  blank_project() |>
  set_info("dados de temperatura max semanal") |>
  set_data(data = df, value = "temp_max", row  = "station.id", col  = "semana", group = "regiao") |>
  set_space(df = stations, label = "station.id", lat = "lat", lon = "lon", alt = "alt", position = "row") |>
  set_prior(type = "normal", semi.conf = F, alpha0 = T) |>
  {\(.) {.$prior$alpha$cov   = car_conditional(.); . }}() |>
  {\(.) {.$prior$alpha$cov   = .$prior$alpha$cov  |> lapply(function(x) x * 400); . }}() |>
  {\(.) {.$prior$lambda$cov  = .$prior$lambda$cov |> lapply(function(x) car(neib_simple(ncol(x)))) ; . }}() |>
  set_fit2(iter = 1000, seed = 12345) |>
  {\(.) {export(., "C:/Users/Carlos/Downloads"); .}}()

