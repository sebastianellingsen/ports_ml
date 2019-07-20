# loading libraries
library(rWind)
library(raster)
library(rworldmap)
library(gdistance)
library(fields)
library(lubridate)
library(shape)

## Preparing and downloading data 
dt <- seq(ymd_hms(paste(2011, 9, 3, 00, 00, 00, sep="-")),
          ymd_hms(paste(2017, 9, 11, 21, 00, 00, sep="-")),
          by="10 days")

wind_series <- wind.dl_2(dt, -110, 20, -90, 80)
wind_series_layer <- wind2raster(wind_series)

# Averaging over the period and making raster file
wind_series_mean <- wind.mean(wind_series)
wind_series_mean_layer <- wind2raster(wind_series_mean)

# Setting overland wind speed to 0
wind_series_mean_layer_masked <- mask(wind_series_mean_layer@layers[[2]], 
                                      countries110, 
                                      inverse=T,
                                      updateNA=F,
                                      updatevalue=0)

# Replacing the windspeeds overland
wind_series_mean_layer@layers[[2]] <- wind_series_mean_layer_masked

# Obtaining the transition matrix
Conductance <- flow.dispersion(wind_series_mean_layer, 
                               output="transitionLayer")

## Port locations
loc <- matrix(c(-6.29465,   -82.366592,  -96.134224, 
                -75.51444,  -77.042793,  -79.516670, 
                -99.912437, -43.182365,  -56.164532,
                -58.381592,  36.52978,    23.113592, 
                 19.173773,  10.39972,   -12.046374, 
                 8.983333,   16.848824,  -22.970722, 
                -34.901112, -34.603722), 
              10, 2)

colnames(loc) <- c("lon", "lat")
rownames(loc) <- c("Cadiz", "Havana", "Veracruz", 
                   "Cartagena", "Lima", "Panama", 
                   "Acapulco", "Rio_De_Janeiro", 
                   "Montevideo", "Buenos_Aires")

shortest_path <- lapply(2:10, 
                        function(x) shortestPath(Conductance, 
                                                 loc[1,], 
                                                 loc[x,],
                                                 output = "SpatialLines"))
cumul_cost    <- lapply(2:10, 
                        function(x) costDistance(Conductance, loc[1,], loc[x,]))






