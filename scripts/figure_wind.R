library("ggquiver")

## Preparing datasets 
coastline10 <- ne_download(scale    = 10, 
                           type     = 'coastline', 
                           category = 'physical')

coastlines_sim2_df <- SpatialLinesDataFrame(coastline10,
                                            coastline10@data) 

wind_series       <- wind.dl_2("2016-08-07 UTC", -110, -60, 0, 40) 

wind_series <- wind_series[["2016-08-07"]]%>% 
  mutate(u = ugrd10m, v = vgrd10m)

wind_series$id = seq(1:dim(wind_series)[1])

## Remove some arrows in the plot 
lat.keep = unique(wind_series$lat)[seq(2, length(unique(wind_series$lat)), 2)]
lon.keep = unique(wind_series$lon)[seq(2, length(unique(wind_series$lon)), 2)]

# Removing points over land 
countries_list <- c("Chile", 
                    "Bolivia", 
                    "Peru", 
                    "Argentina", 
                    "Uruguay",   
                    "Ecuador", 
                    "Colombia", 
                    "Paraguay", 
                    "Venezuela", 
                    "Panama",
                    "El Salvador", 
                    "Honduras", 
                    "Costa Rica", 
                    "Guatemala", 
                    "Mexico", 
                    "Nicaragua", 
                    "Cuba", 
                    "Dominican Republic",
                    'United States of America')

countries10 <- ne_download(scale = 50, 
                           type = 'countries', 
                           category = 'cultural')

countries10 <- countries10[countries10@data$ADMIN %in% countries_list, ]

coords          <- cbind(wind_series$lon, wind_series$lat)
sp              <- SpatialPoints(coords)
cities_tmp      <- SpatialPointsDataFrame(coords, wind_series)
crs(cities_tmp) <- crs(wind_series)

cities_tmp <- intersect(cities_tmp, countries10)
wind_series <- wind_series %>% 
  filter(!(id %in% cities_tmp@data$id))

## Plot the least cost travel routes 
## loaing the data 
wind_series_mean_layer <- stack('../output/wind_series_mean_layer.grd')
wind_series_mean_layer@layers[[1]]@data@names <- 'direction'
wind_series_mean_layer@layers[[2]]@data@names <- 'speed'

## Preparing dataset on ports
source("../scripts/data_prep_ports.R")
ports       <- spTransform(ports, crs(wind_series_mean_layer))
ports_spain <- spTransform(ports_spain, crs(wind_series_mean_layer))

# Primary ports
cadiz     <- ports_spain[ports_spain@data$ports=="Cadiz", ]
veracruz  <- ports[ports@data$ports=="Veracruz", ]

# Obtaining the transition matrix
Conductance <- flow.dispersion(wind_series_mean_layer, 
                               output="transitionLayer")

cadiz_veracruz <- shortestPath(Conductance, cadiz, veracruz, output = "SpatialLines")
veracruz_cadiz <- shortestPath(Conductance, veracruz, cadiz, output = "SpatialLines")

cadiz_veracruz_df <- SpatialLinesDataFrame(cadiz_veracruz,
                                           as.data.frame(cadiz_veracruz@lines[[1]]@ID))

veracruz_cadiz_df <- SpatialLinesDataFrame(veracruz_cadiz,
                                           as.data.frame(veracruz_cadiz@lines[[1]]@ID))




