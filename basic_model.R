basic_model <- nimbleCode({
  
  alpha ~ dnorm(0,0.001)
  beta ~ dnorm(0,0.001)
  
  for (i in 1:N) {
    Observed[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + log(E[i]) + beta*SEc[i]
  } 
  
})
