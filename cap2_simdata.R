devtools::load_all("D:/carlos/01_pesquisa/fastan")
library(tidyverse)

generate_data_sc = function(rows.by.group, columns, cicles = 1, semi.conf = F) {
  stopifnot(
    "if the model is semi.conf there
    must be at least three groups" = ifelse(semi.conf, length(rows.by.group) >= 3, T)
  )
  
  n.fac = length(rows.by.group) - as.integer(semi.conf)
  
  groups_limits = fiat_groups_limits(rows.by.group)
  
  alpha = matrix(0,
                 nrow = sum(rows.by.group),
                 ncol = n.fac)
  
  lambda = matrix(0,
                  ncol = columns,
                  nrow = n.fac)
  
  for (i in 1:n.fac) {
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], i] = stats::runif(rows.by.group[i], 3, 8)
    
    lambda[i, ] = stats::rnorm(columns, 4, 1) |> sort(decreasing = as.logical(i %/% 2))
  }
  
  if (semi.conf) {
    i = i + 1
    alpha[groups_limits[[1]][i] : groups_limits[[2]][i], ] =
      matrix(
        stats::runif(rows.by.group[i] * n.fac, 3, 8),
        nrow = rows.by.group[i],
        ncol = n.fac
      )
  }
  
  sigma2 = stats::runif(sum(rows.by.group), .5, 30)
  
  alpha  = alpha  |> abs()
  lambda = lambda |> abs()
  
  epsilon = matrix(
    stats::rnorm(sum(rows.by.group)*columns*cicles, 0, sqrt(sigma2)) ,
    ncol = columns*cicles,
    byrow = F
  )
  
  alpha_lambda = alpha %*% lambda |>
    {\(.)
      do.call(cbind, lapply(1:ncol(.), function(i) {
        matrix(rep(.[, i], cicles), nrow = nrow(.))
      }))
    }()
  
  x = (alpha_lambda + epsilon) |>
    as.data.frame() |>
    dplyr::mutate(
      row = paste0("row ", 1:sum(rows.by.group)),
      group = paste0("group ", 1:n.fac) |>
        {\(.) if(semi.conf)
          rep(., utils::head(rows.by.group, -1)) |>
            c("group extra" |> rep(utils::tail(rows.by.group, 1)))
          else
            rep(., rows.by.group)
        }()
    ) |>
    `colnames<-`(
      paste0("level ", 1:columns) |>
        rep(each = cicles) |>
        append(c("row", "group")) |>
        make.unique()
    ) |>
    tidyr::pivot_longer(cols = 1:{\()columns*cicles}() , names_to = "col", values_to = "value") |>
    {\(.)
      dplyr::mutate(.,
                    col = sapply(.$col, function(x) { strsplit(x, "[.]") |> purrr::pluck(1) |> purrr::pluck(1)}) |>
                      {\(.) factor(., levels = unique(.) )}(),
                    group = factor(.$group, levels = unique(.$group)),
                    row   = factor(.$row,   levels = unique(.$row))
      )}()
  
  mod = model_data_sc(x, "value", "group", "row", "col", semi.conf)
  mod$real = list(alpha = alpha,
                  lambda = lambda,
                  sigma2 = as.matrix(sigma2)
  )
  return(mod)
}


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
simdata1$fit     = fastan::run_stan(simdata1$model, iter = 6000, warmup = 2000, seed = 12345)
simdata1$summary = fastan::summary_matrix(simdata1$fit, simdata1$model)

c = 3
simdata1$summary$lambda[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")] = c * simdata1$summary$lambda[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]
simdata1$summary$alpha[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp")]  = (1/c) * simdata1$summary$alpha[,,c("mean", "median", "sd", "hpd_min", "hpd_max", "hpd_amp"), drop = F]

saveRDS(simdata1, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_simdata_exploratorio.rds")
fastan::percentage_hits(simdata1$summary)


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
simdata2$fit     = fastan::run_stan(simdata2$model, iter = 6000, warmup = 2000, seed = 12345)
simdata2$summary = fastan::summary_matrix(simdata2$fit, simdata2$model)

saveRDS(simdata2, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_simdata_confirmatorio.rds")
fastan::percentage_hits(simdata2$summary)


##################################
#####   Semi-confirmatório   #####
##################################

set.seed(12345)

cols = max(df$semana)
rows.by.group = df$alt_tipo |> table() |> unname() |> {\(.) ./cols}()

simdata3       = list()
simdata3$info  = "Dados simulados com mesmo tamanho que dados reais de temp. max.; modelo semi-confirmatório"
simdata3$model =
  fastan::generate_data_sc(
    rows.by.group = rows.by.group,
    columns = cols,
    semi.conf = T
    )
simdata3$fit     = fastan::run_stan(simdata3$model, iter = 6000, warmup = 2000, seed = 12345)
simdata3$summary = fastan::summary_matrix(simdata3$fit, simdata3$model)

saveRDS(simdata3, "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap2_simdata_semi_confirmatorio.rds")
fastan::percentage_hits(simdata3$summary)
