spatial_lasso_model <- nimbleCode({
  
  alpha ~ dnorm(0,0.001)
  beta ~ dnorm(0,0.001)
  beta_spatial ~ ddexp(0,lambda_tau)
  lambda_tau <- lambda_lasso*tau
  tau ~ dgamma(0.0001,0.0001)
  
  # Null model
  
  for (i in 1:N) {
    Observed[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + log(E[i]) + beta*SEc[i]
  } 
  
  # Spatial model
  
  for (i in 1:N) {
    Observed[N+i] ~ dpois(lambda[N+i])
    log(lambda[N+i]) <- alpha + log(E[i]) + (beta + beta_spatial)*SEc[i] + u[i] + v[i]
  } 
  
  # Unstructured spatial effect
  for (i in 1:N_polygons) {
    v[i] ~ dnorm(0,tau.v)
  }
  sigma2.v ~ dgamma(1, 0.01)
  tau.v <- 1/sigma2.v
  
  # Structured spatial effect
  u[1:N_polygons] ~ dcar_normal(adj_polygons[1:l_adj_polygons], 
                                weights_polygons[1:l_adj_polygons], 
                                num_polygons[1:N_polygons], tau.u, zero_mean = 1)
  sigma2.u ~ dgamma(1, 0.01)
  tau.u <- 1/sigma2.u
  
})