library(nimble)
library(sp)
library(sf)
library(spdep)
source("NIMBLE_codes/spatial_lasso_model.R")

load("slovenia.rda")
dataset=as.data.frame(slovenia)
dataset=dataset[,-ncol(dataset)]

polygons=as(slovenia$geometry,"Spatial")
proj4string(polygons)="+proj=utm +zone=30 ellps=WGS84"
plot(polygons)
polygons=SpatialPolygonsDataFrame(polygons,data=data.frame(SpUnit=1:length(polygons)),match.ID = F)
polygons_nb=poly2nb(polygons,snap=1.5)
polygons_WB=nb2WB(polygons_nb)

log_lambda=seq(-2,6,1)
log_lambda=4
for (lambda_lasso in 10^(log_lambda)){
  
  if (!file.exists(paste0("Models/spatial_lasso_model_",lambda_lasso,".rda"))){
    
    constants <- list(
      
      N = nrow(dataset),
      E = dataset$E,
      SEc = dataset$SEc,
      
      N_polygons = length(polygons),  
      adj_polygons = polygons_WB$adj,                             
      num_polygons = polygons_WB$num,
      weights_polygons = polygons_WB$weights,
      l_adj_polygons = length(polygons_WB$adj),
      
      lambda_lasso = lambda_lasso
      
    )
    
    # Fit model
    
    data <- list(Observed = c(dataset$O,dataset$O))
    inits <- function() list(alpha = 0,
                             beta = 0,
                             beta_spatial = 0,
                             tau = 0.1,
                             sigma2.u=0.1,
                             sigma2.v=0.1,
                             u=rep(0,constants$N_polygons),
                             v=rep(0,constants$N_polygons))
    mcmc.output <- nimbleMCMC(spatial_lasso_model, data = data, inits = inits, constants = constants,
                              monitors = c("alpha", 
                                           "beta",
                                           "beta_spatial",
                                           "tau",
                                           "u",
                                           "v",
                                           "lambda"), thin = 10,
                              niter = 50000, nburnin = 25000, nchains = 2, 
                              summary = TRUE, WAIC = TRUE)
    save(mcmc.output,file=paste0("Models/spatial_lasso_model_",lambda_lasso,".rda"))
    
  }
  
  
}

