set.seed(12345)

a = b = .1
m0 = m1 = m2 = 0
v0 = v1 = v2 = 10

phi = 1
beta0 = 1
beta1 = 1
beta2 = 1

phi = beta0 = beta1 = beta2 = 50

sample_chain = data.frame(
  phi = c(phi),
  beta0 = c(beta0),
  beta1 = c(beta1),
  beta2 = c(beta2)
)

rodadas = 1e4

for (rodada in 1:rodadas){
  delta_phi = sum(( y - x %*% c(beta0, beta1, beta2) )^2)
  phi = rgamma(1, n/2 + a, (delta_phi / 2 + b)^-1)  # ajuste no parâmetro de taxa-escala
  
  delta0 = sum( x[,2:3] %*% c(beta1, beta2) - y )
  lambda0 = ( phi * n + v0^-1 )^-1
  beta0 = rnorm(1, lambda0 * ( m0 * v0^-1 - phi * delta0 ), sqrt(lambda0))
  
  delta1 = sum( x[,2] * (x[,c(1,3)] %*% c(beta0, beta2) - y) )
  lambda1 = ( phi * sum(x[,2]^2) + v1^-1 )^-1
  beta1 = rnorm(1, lambda1 * ( m1 - delta1 ), sqrt(lambda1))
  
  delta2 = sum( x[,3] * (x[,c(1,2)] %*% c(beta0, beta1) - y) )
  lambda2 = ( phi * sum(x[,3]^2) + v2^-1 )^-1
  beta2 = rnorm(1, lambda2 * ( m2 - delta2 ), sqrt(lambda2))

  sample_chain[nrow(sample_chain) + 1,] = list(phi, beta0, beta1, beta2)
}

original = sample_chain
sample_chain = original
sample_chain = sample_chain[seq(rodadas*.3, nrow(sample_chain)),]

# plot cadeia
par(mfrow=c(2,2))
for (col in colnames(sample_chain)){
  plot(sample_chain[,col],
       type="l",
       ylab="value",
       main=col
       )
}

# plot densidade
prob = 0.05  # correção
par(mfrow=c(2,2))
for (col in colnames(sample_chain)){
  intervalo = quantile(sample_chain[,col], probs=c(prob/2, 1-prob/2))
  teste = (sample_chain[,col] > intervalo[[1]]) & (sample_chain[,col] < intervalo[[2]])

  plot(density( sample_chain[teste, col] ),
       type="l",
       ylab="density",
       main=col
       )
}

plot_density = function(df){
  prob = 0.05
  par(mfrow=c(2,2))
  for (col in colnames(sample_chain)){
    intervalo = quantile(sample_chain[,col], probs=c(prob/2, 1-prob/2))
    teste = (sample_chain[,col] > intervalo[[1]]) & (sample_chain[,col] < intervalo[[2]])
    
    plot(density( length(sample_chain[teste, col]) ),
         type="l",
         ylab="density",
         main=col
    )
  }
  
}


correct_quantile = function(df, prob=.05){
  m = matrix(, nrow = nrow(df)*(1 - prob), ncol = 0)
  new_df = data.frame()
  for (col in colnames(df)){
    col = colnames(df)[i]
    intervalo = quantile(df[,col], probs=c(prob/2, 1-prob/2))
    teste = (df[,col] > intervalo[[1]]) & (df[,col] < intervalo[[2]])
    m = cbind(m, df[teste, col])
  }
  return(m)
}

correct_quantile(sample_chain)
df = sample_chain
