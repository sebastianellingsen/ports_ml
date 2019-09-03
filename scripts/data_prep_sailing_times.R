# loading libraries
library(rWind)
library(raster)
library(rworldmap)
library(gdistance)
library(fields)
library(lubridate)
library(shape)

# south_america <- readOGR("../output/south_america_grid.shp", "south_america_grid")

## Preparing and downloading data 
dt <- seq(ymd_hms(paste(2011, 9, 3,  00, 00, 00, sep = "-")),
          ymd_hms(paste(2017, 9, 11, 21, 00, 00, sep = "-")),
          by="300 days")

wind_series <- wind.dl_2(dt, -140, 20, -90, 80)
wind_series_layer <- wind2raster(wind_series)

# Averaging over the period and making raster file
wind_series_mean <- wind.mean(wind_series)
wind_series_mean_layer <- wind2raster(wind_series_mean)

countries110 <- ne_download(scale = 110, 
                            type = 'countries', 
                            category = 'cultural')

countries110 <- spTransform(countries110, crs(wind_series_mean_layer))

# Setting overland wind speed to 0
wind_series_mean_layer_masked <- raster::mask(wind_series_mean_layer@layers[[2]], 
                                              countries110, 
                                              inverse=TRUE,
                                              updateNA=F,
                                              updatevalue=0)

# Replacing cell at Gibraltar
wind_series_mean_layer_masked[23178] <- 6

# Replacing the windspeeds overland
wind_series_mean_layer@layers[[2]] <- wind_series_mean_layer_masked

# Obtaining the transition matrix
Conductance <- flow.dispersion(wind_series_mean_layer, 
                               output="transitionLayer", 
                               type="active")

# Spain
Cadiz     <- c(-6.29465, 36.52978)
Alicante  <- c(-0.48149, 38.04517)
Barcelona <- c(2.26899, 41.38879)
A_Coruña  <- c(-8.411540, 43.362343)

# New world
Havana         <- c(-82.366592, 23.513592)
Veracruz       <- c(-96.134224, 19.173773)
Cartagena      <- c(-75.51444, 10.39972)
Lima           <- c(-77.042793, -12.846374)
Panama         <- c(-79.516670, 8.583333)
Acapulco       <- c(-99.912437, 16.448824)
Rio_De_Janeiro <- c(-43.182365, -22.970722)
Montevideo     <- c(-56.164532, -34.901112)
Buenos_Aires   <- c(-58.081592, -34.603722)

## Port locations
loc <- matrix(c(Cadiz[1],       Alicante[1],       Barcelona[1], 
                A_Coruña[1],    Havana[1],         Veracruz[1], 
                Cartagena[1],   Lima[1],           Panama[1],   
                Acapulco[1],    Rio_De_Janeiro[1], Montevideo[1], 
                Buenos_Aires[1],
                Cadiz[2],       Alicante[2],       Barcelona[2], 
                A_Coruña[2],    Havana[2],         Veracruz[2], 
                Cartagena[2],   Lima[2],           Panama[2], 
                Acapulco[2],    Rio_De_Janeiro[2], Montevideo[2], 
                Buenos_Aires[2]), 
              13, 2)

colnames(loc) <- c("lon", "lat")
rownames(loc) <- c("Cadiz", "Alicante", "Barcelona", 
                   "A_Coruña", "Havana", "Veracruz", 
                   "Cartagena", "Lima", "Panama", 
                   "Acapulco", "Rio_De_Janeiro", 
                   "Montevideo", "Buenos_Aires")

least_cost_path <-  function(from, to){
  
  path <- shortestPath(Conductance, 
                       loc[from,], 
                       loc[to,],
                       output = "SpatialLines")
  
  return(list(path))
}

## Costs and paths from Cadiz
shortest_path_from_cadiz <- lapply(5:13, 
                               function(x) least_cost_path(1, x)) %>% 
  unlist() %>% do.call(bind, .) 

## Shortest path to cadiz
shortest_path_to_cadiz <- lapply(5:13,
                               function(x) least_cost_path(x, 1)) %>%
  unlist() %>% do.call(bind, .)


# Generating a dataframe of sailing times

least_cost <-  function(from, to){
  
  cost <- costDistance(Conductance, 
                       loc[from,], 
                       loc[to,])
  return(list(cost))
}

least_cost_from_cadiz <- lapply(5:13, 
                                   function(x) least_cost(1, x)) %>% 
  unlist() 

## Shortest path to cadiz
least_cost_to_cadiz <- lapply(5:13, 
                                 function(x) least_cost(x, 1)) %>% 
  unlist()  

least_costs <- c(least_cost_from_cadiz, least_cost_to_cadiz)

VoyageFrom <- c(rep("Cadiz", 9), rownames(loc)[5:13])
VoyageTo <- c(rownames(loc)[5:13], rep("Cadiz", 9))

least_costs <- cbind(VoyageFrom, VoyageTo, least_costs) %>% as.data.frame()

## Distance to coastal grid cells
south_america_unprojected <- spTransform(south_america, 
                                         "+proj=longlat +datum=WGS84 +ellps=WGS84")
sailing_time <- function(x){
  cell <- south_america_unprojected[x,]
  return(ifelse(cell@data$coast_ds==0, costDistance(Conductance, 
                                           loc[1,], 
                                           coordinates(cell)), 0))
}




