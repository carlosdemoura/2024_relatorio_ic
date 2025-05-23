img = function(x) {paste0("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/img/", x)}


generate_data_sc = function(rows.by.group, columns, semi.conf = F, factors.alike = F) {
  normalize = function(x) {
    norm = (x - min(x)) / (max(x)-min(x))
    norm[which(norm==0)] = min(norm[norm > 0]) / 2
    norm
  }
  
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
  }
  
  if (factors.alike) {
    
    for (i in 1:n.fac) {
      lambda[i, ] = stats::rnorm(columns, 4, 1) |> sort(decreasing = as.logical(i %/% 2))
    }
    lambda = lambda |> abs()
    
  } else {
    
    for (i in 1:n.fac) {
      for (j in 2:columns) {
        lambda[i,j] = rnorm(1, lambda[i,j-1])
      }
    }
    lambda =
      lambda |>
      apply(1, normalize) |>
      t()
    
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
  
  sigma2 = stats::runif(sum(rows.by.group), 1, 35)
  
  epsilon = matrix(
    stats::rnorm(sum(rows.by.group)*columns, 0, sqrt(sigma2)) ,
    ncol = columns,
    byrow = F
  )
  
  x = ((alpha %*% lambda) + epsilon) |>
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
        append(c("row", "group")) |>
        make.unique()
    ) |>
    tidyr::pivot_longer(cols = 1:columns, names_to = "col", values_to = "value") |>
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


runif_or_0 = function(n, min = 0, max = 1, p0 = .5) {
  n0 = floor(p0 * n)
  x = 
    rep(0, n0) |>
    c(runif(n - n0, min, max)) |>
    sample(n)
  return(x)
}


get_gewekes = function(fit) {
  fit |>
    rstan::extract(permuted = F) |>
    apply(c(2, 3), function(x) {coda::geweke.diag(x) |> purrr::pluck(1) |> unname()} ) |>
    as.vector()
}
