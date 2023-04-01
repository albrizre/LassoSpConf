basic_model <- nimbleCode({
  
  alpha ~ dnorm(0,0.001)
  beta ~ dnorm(0,0.001)
  
  for (i in 1:N) {
    Observed[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + log(E[i]) + beta*SEc[i]
  } 
  
  # # Unstructured week effect
  # for (i in 1:N_W){
  #   epsilon[i] ~ dnorm(0,tau.epsilon)
  # }
  # sigma2.epsilon ~ dgamma(1,0.5)
  # tau.epsilon <- 1/sigma2.epsilon
  # 
  # # RW2 prior on the effect of week
  # delta[1:N_W] ~ dcar_normal(adj[1:N_W_adj], weights[1:N_W_adj], num[1:N_W], tau.delta, zero_mean = 1)
  # sigma2.delta ~ dgamma(1,0.5)
  # tau.delta <- 1/sigma2.delta
  # 
  # # Unstructured spatial effect
  # for (i in 1:G) {
  #   v[i] ~ dnorm(0,tau.v)
  # }
  # sigma2.v ~ dgamma(1, 0.01)
  # tau.v <- 1/sigma2.v
  # 
  # # Structured spatial effect
  # u[1:G] ~ dcar_normal(adj_grid[1:N_grid_w], weights_grid[1:N_grid_w], num_grid[1:G], tau.u, zero_mean = 1) 
  # sigma2.u ~ dgamma(1, 0.01)
  # tau.u <- 1/sigma2.u
  # 
  # # Space-time effect
  # # for (i in 1:N_INT){
  # #   phi[i] ~ dnorm(0,tau.phi)
  # # }
  # # sigma2.phi ~ dgamma(1, 0.01)
  # # tau.phi <- 1/sigma2.phi
  
})
