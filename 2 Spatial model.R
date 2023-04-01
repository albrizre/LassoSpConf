library(nimble)
library(sp)
library(rgdal)
library(rgeos)
library(sf)
library(spdep)
source("NIMBLE_codes/spatial_model.R")

load("slovenia.rda")
dataset=as.data.frame(slovenia)
dataset=dataset[,-ncol(dataset)]

polygons=as(slovenia$geometry,"Spatial")
proj4string(polygons)="+proj=utm +zone=30 ellps=WGS84"
plot(polygons)
polygons=SpatialPolygonsDataFrame(polygons,data=data.frame(SpUnit=1:length(polygons)),match.ID = F)
polygons_nb=poly2nb(polygons,snap=1.5)
polygons_WB=nb2WB(polygons_nb)

constants <- list(
  
  N = nrow(dataset),
  E = dataset$E,
  SEc = dataset$SEc,
  
  N_polygons = length(polygons),  
  adj_polygons = polygons_WB$adj,                             
  num_polygons = polygons_WB$num,
  weights_polygons = polygons_WB$weights,
  l_adj_polygons = length(polygons_WB$adj)

)

# Fit model

data <- list(Observed = dataset$O)
inits <- function() list(alpha = 0,
                         beta = 0,
                         sigma2.u=0.1,
                         sigma2.v=0.1,
                         u=rep(0,constants$N_polygons),
                         v=rep(0,constants$N_polygons))
mcmc.output <- nimbleMCMC(spatial_model, data = data, inits = inits, constants = constants,
                          monitors = c("alpha", 
                                       "beta",
                                       "u",
                                       "v",
                                       "lambda"), thin = 10,
                          niter = 20000, nburnin = 4000, nchains = 1, 
                          summary = TRUE, WAIC = TRUE)
save(mcmc.output,file="Models/spatial_model.rda")