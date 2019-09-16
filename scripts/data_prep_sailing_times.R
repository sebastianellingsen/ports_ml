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
          by="30 days")

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
gibraltar <- cbind(lat = -5.353585, lon = 36.140751)
gibraltar <- SpatialPoints(coords = gibraltar, 
                            proj4string = crs(wind_series_mean_layer))

wind_series_mean_layer_masked[cellFromXY(wind_series_mean_layer_masked,gibraltar)] <- 6
# wind_series_mean_layer_masked[23178] <- 6

# Replacing the windspeeds overland
wind_series_mean_layer@layers[[2]] <- wind_series_mean_layer_masked

# Obtaining the transition matrix
Conductance <- flow.dispersion(wind_series_mean_layer, 
                               output="transitionLayer", 
                               type="active")

# Spain
Cadiz         <- c(ports_spain[ports_spain@data$ports=="Cadiz",]$lon, 
                   ports_spain[ports_spain@data$ports=="Cadiz",]$lat)
La_Coruna     <- c(ports_spain[ports_spain@data$ports=="La Coruna",]$lon, 
                   ports_spain[ports_spain@data$ports=="La Coruna",]$lat)
Alicante      <- c(ports_spain[ports_spain@data$ports=="Alicante",]$lon, 
                   ports_spain[ports_spain@data$ports=="Alicante",]$lat)
Barcelona     <- c(ports_spain[ports_spain@data$ports=="Barcelona",]$lon, 
                   ports_spain[ports_spain@data$ports=="Barcelona",]$lat)

# New world 
Havana         <- c(ports[ports@data$ports=="Havana",]$lon, 
                    ports[ports@data$ports=="Havana",]$lat)
Veracruz       <- c(ports[ports@data$ports=="Veracruz",]$lon, 
                    ports[ports@data$ports=="Veracruz",]$lat)
Cartagena      <- c(ports[ports@data$ports=="Cartagena",]$lon, 
                    ports[ports@data$ports=="Cartagena",]$lat)
Lima           <- c(ports[ports@data$ports=="Lima",]$lon, 
                    ports[ports@data$ports=="Lima",]$lat)
Panama         <- c(ports[ports@data$ports=="Panama",]$lon, 
                    ports[ports@data$ports=="Panama",]$lat)
Acapulco       <- c(ports[ports@data$ports=="Acapulco",]$lon, 
                    ports[ports@data$ports=="Acapulco",]$lat)
Montevideo     <- c(ports[ports@data$ports=="Montevideo",]$lon, 
                    ports[ports@data$ports=="Montevideo",]$lat)
Buenos_Aires   <- c(ports[ports@data$ports=="Buenos Aires",]$lon, 
                    ports[ports@data$ports=="Buenos Aires",]$lat)


## Port locations
loc <- matrix(c(Cadiz[1],       Alicante[1],       Barcelona[1], 
                La_Coruna[1],    Havana[1],         Veracruz[1], 
                Cartagena[1],   Lima[1],           Panama[1],   
                Acapulco[1],    Montevideo[1],     Buenos_Aires[1],
                Cadiz[2],       Alicante[2],       Barcelona[2], 
                La_Coruna[2],    Havana[2],         Veracruz[2], 
                Cartagena[2],   Lima[2],           Panama[2], 
                Acapulco[2],    Montevideo[2],     Buenos_Aires[2]), 
              12, 2)

colnames(loc) <- c("lon", "lat")
rownames(loc) <- c("Cadiz", "Alicante", "Barcelona", 
                   "La Coruna", "Havana", "Veracruz", 
                   "Cartagena", "Lima", "Panama", 
                   "Acapulco", "Montevideo", "Buenos_Aires")

least_cost_path <-  function(from, to){
  
  path <- shortestPath(Conductance, 
                       loc[from,], 
                       loc[to,],
                       output = "SpatialLines")
  
  return(list(path))
}

## Cadiz
shortest_path_from_cadiz <- lapply(5:12, 
                                   function(x) least_cost_path(1, x)) %>% 
  unlist() %>% do.call(bind, .) 
shortest_path_to_cadiz <- lapply(5:12,
                                 function(x) least_cost_path(x, 1)) %>%
  unlist() %>% do.call(bind, .)

## La Coruna
shortest_path_from_coruna <- lapply(5:12, 
                                   function(x) least_cost_path(4, x)) %>% 
  unlist() %>% do.call(bind, .) 
shortest_path_to_coruna <- lapply(5:12,
                                 function(x) least_cost_path(x, 4)) %>%
  unlist() %>% do.call(bind, .)

## Barcelona
shortest_path_from_barcelona <- lapply(5:12, 
                                    function(x) least_cost_path(3, x)) %>% 
  unlist() %>% do.call(bind, .) 
shortest_path_to_barcelona <- lapply(5:12,
                                  function(x) least_cost_path(x, 3)) %>%
  unlist() %>% do.call(bind, .)

## Havana 
shortest_path_from_havana <- lapply(1:4, 
                                    function(x) least_cost_path(5, x)) %>% 
  unlist() %>% do.call(bind, .) 

## Veracruz
shortest_path_from_veracruz <- lapply(1:4, 
                                       function(x) least_cost_path(6, x)) %>% 
  unlist() %>% do.call(bind, .) 

## Cartagena 
shortest_path_from_cartagena <- lapply(1:4, 
                                      function(x) least_cost_path(7, x)) %>% 
  unlist() %>% do.call(bind, .) 

## Lima 
shortest_path_from_lima <- lapply(1:4, 
                                    function(x) least_cost_path(8, x)) %>% 
  unlist() %>% do.call(bind, .) 

## Montevideo 
shortest_path_from_montevideo <- lapply(1:4, 
                                  function(x) least_cost_path(11, x)) %>% 
  unlist() %>% do.call(bind, .) 

## Buenos Aires 
shortest_path_from_montevideo <- lapply(1:4, 
                                        function(x) least_cost_path(12, x)) %>% 
  unlist() %>% do.call(bind, .) 



## Generating a dataframe of sailing times

# Function that calculates the least cost path
least_cost <-  function(from, to){
  
  cost <- costDistance(Conductance, 
                       loc[from,], 
                       loc[to,])
  
  return(list(cost))
}

## Cadiz
shortest_path_from_cadiz <- lapply(5:12, 
                                   function(x) least_cost(1, x)) %>%  unlist() 

shortest_path_to_cadiz <- lapply(5:12,
                                 function(x) least_cost(x, 1)) %>% unlist() 

least_costs_fromcadiz<- cbind(VoyageFrom  = c(rep("Cadiz", 4)), 
                                VoyageTo    = c(rownames(loc)[5:12]), 
                                least_costs = c(shortest_path_from_cadiz)) 

least_costs_tocadiz <- cbind(VoyageFrom  = c(rownames(loc)[5:12]), 
                              VoyageTo    = c(rep("Cadiz", 4)), 
                              least_costs = c(shortest_path_to_cadiz)) 

least_costs_cadiz <- rbind(least_costs_fromcadiz, 
                           least_costs_tocadiz) 

## La Coruna
shortest_path_from_coruna <- lapply(5:12, 
                                    function(x) least_cost(4, x)) %>% unlist() 
shortest_path_to_coruna <- lapply(5:12,
                                  function(x) least_cost(x, 4)) %>% unlist() 

least_costs_fromcoruna <- cbind(VoyageFrom     = c(rep("La Coruna", 4)), 
                                   VoyageTo    = c(rownames(loc)[5:12]), 
                                   least_costs = c(shortest_path_from_coruna)) 

least_costs_tocoruna <- cbind(VoyageFrom     = c(rownames(loc)[5:12]), 
                                 VoyageTo    = c(rep("La Coruna", 4)), 
                                 least_costs = c(shortest_path_to_coruna)) 

least_costs_coruna <- rbind(least_costs_fromcoruna, 
                            least_costs_tocoruna) 

## Barcelona
shortest_path_from_barcelona <- lapply(5:12, 
                                       function(x) least_cost(3, x)) %>% unlist() 
shortest_path_to_barcelona <- lapply(5:12,
                                     function(x) least_cost(x, 3)) %>% unlist()

least_costs_frombarcelona <- cbind(VoyageFrom  = c(rep("Barcelona", 4)), 
                                   VoyageTo    = c(rownames(loc)[5:12]), 
                                   least_costs = c(shortest_path_from_havana)) 

least_costs_tobarcelona <- cbind(VoyageFrom  = c(rownames(loc)[5:12]), 
                                 VoyageTo    = c(rep("Barcelona", 4)), 
                                 least_costs = c(shortest_path_to_barcelona)) 

least_costs_barcelona <- rbind(least_costs_frombarcelona, 
                               least_costs_tobarcelona) 

least_costs <- rbind(least_costs_cadiz, least_costs_coruna) %>% 
  as.data.frame() %>% mutate(least_costs = as.numeric(as.character(least_costs)))


## Distance to coastal grid cells
# south_america_unprojected <- spTransform(south_america, 
#                                          "+proj=longlat +datum=WGS84 +ellps=WGS84")
# sailing_time <- function(x){
#   cell <- south_america_unprojected[x,]
#   return(ifelse(cell@data$coast_ds==0, costDistance(Conductance, 
#                                                     loc[1,], 
#                                                     coordinates(cell)), 0))
# }



