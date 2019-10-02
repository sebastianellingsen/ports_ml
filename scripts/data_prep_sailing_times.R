## Preparing packages
if (!require(pacman)) install.packages("pacman")
p_load(rWind,
       raster,
       gdistance,
       fields,
       lubridate,
       shape)

# south_america <- readOGR("../output/south_america_grid.shp", "south_america_grid")

## Preparing and downloading data 
dt <- seq(ymd_hms(paste(2011, 9, 3,  00, 00, 00, sep = "-")),
          ymd_hms(paste(2017, 9, 11, 21, 00, 00, sep = "-")),
          by="10 days")

wind_series       <- wind.dl_2(dt, -170, 170, -90, 80)
wind_series_layer <- wind2raster(wind_series)

# Averaging over the period and making raster file
wind_series_mean       <- wind.mean(wind_series)
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

# Replacing the windspeeds overland
wind_series_mean_layer@layers[[2]] <- wind_series_mean_layer_masked

# Obtaining the transition matrix
Conductance <- flow.dispersion(wind_series_mean_layer, 
                               output="transitionLayer", 
                               type="active")



