set.seed(12345)

a = b = .1
m0 = m1 = m2 = 0
v0 = v1 = v2 = 10

real = 
  list(
    phi = 2,
    beta0 = 3,
    beta1 = 1,
    beta2 = 1/2
  )

n = 100
x =
  c(rep(1, n), rnorm(n), rbinom(n, 1, 1/2)) |>
  matrix(ncol = 3)
y = x %*% c(real$beta0, real$beta1, real$beta2) + rnorm(n, 0, sqrt(real$phi))

chain =
  data.frame(rep(1,4) |> as.matrix() |> t()) |>
  `colnames<-`(c("phi", "beta0", "beta1", "beta2"))

rodadas = 1e3

beta0 = beta1 = beta2 = 1
for (rodada in 1:rodadas){
  delta_phi = sum( ( y - x %*% c(beta0, beta1, beta2) )^2 )
  phi = 1/rgamma(1, n/2 + a, rate = delta_phi / 2 + b)
  
  delta0 = sum( x[,2:3] %*% c(beta1, beta2) - y )
  lambda0 = ( phi^-1 * n + v0^-1 )^-1
  beta0 = rnorm(1, lambda0 * ( m0 * v0^-1 - phi^-1 * delta0 ), sqrt(lambda0))
  
  delta1 = sum( x[,2] * (x[,c(1,3)] %*% c(beta0, beta2) - y) )
  lambda1 = ( phi^-1 * sum(x[,2]^2) + v1^-1 )^-1
  beta1 = rnorm(1, lambda1 * ( m1 - delta1 ), sqrt(lambda1))
  
  delta2 = sum( x[,3] * (x[,c(1,2)] %*% c(beta0, beta1) - y) )
  lambda2 = ( phi^-1 * sum(x[,3]^2) + v2^-1 )^-1
  beta2 = rnorm(1, lambda2 * ( m2 - delta2 ), sqrt(lambda2))

  chain[nrow(chain) + 1,] = c(phi, beta0, beta1, beta2)
}

chain = chain[(rodadas/2):rodadas,]

par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(density(chain[, i]),
       main = colnames(chain)[i],
       xlab = "Valor",
       ylab = "Densidade",
       col = "darkblue",
       lwd = 2)
  abline(v = real[i], col = "red", lwd = 2, lty = 2)
}

par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(chain[, i],
       type = "l",
       main = paste("Cadeia de", colnames(chain)[i]),
       xlab = "Iteração",
       ylab = "Valor",
       col = "darkgreen")
  abline(h = real[i], col = "red", lwd = 2, lty = 2)
}
