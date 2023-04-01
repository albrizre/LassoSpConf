library(nimble)
source("NIMBLE_codes/basic_model.R")

load("slovenia.rda")
dataset=as.data.frame(slovenia)
dataset=dataset[,-ncol(dataset)]

constants <- list(
  
  N = nrow(dataset),
  E = dataset$E,
  SEc = dataset$SEc
  
)

# Fit model

data <- list(Observed = dataset$O)
inits <- function() list(alpha = 0,
                         beta = 0)
mcmc.output <- nimbleMCMC(basic_model, data = data, inits = inits, constants = constants,
                          monitors = c("alpha", 
                                       "beta",
                                       "lambda"), thin = 10,
                          niter = 20000, nburnin = 4000, nchains = 1, 
                          summary = TRUE, WAIC = TRUE)
save(mcmc.output,file="Models/basic_model.rda")