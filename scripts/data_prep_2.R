## This file prepares data for predicting using measures of the terrain directly


################################################################################
######################### Preparing data #######################################
################################################################################

## Preparing packages
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, 
       sf, 
       raster, 
       tmap, 
       sp, 
       rgdal, 
       rnaturalearth, 
       rgeos, 
       viridis, 
       tictoc, 
       spatialEco, 
       ranger, 
       caret)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
bwidth <- 15

## Ports 
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[!is.na(ports$HARBORSIZE),]
ports <- ports[ports$HARBORSIZE!="V",]


## Coastline
coastline10 <- ne_download(scale = 10, 
                           type = 'coastline', 
                           category = 'physical')


## Countries
countries10 <- ne_download(scale = 10, 
                           type = 'countries', 
                           category = 'cultural')

countries110 <- ne_download(scale = 110, 
                            type = 'countries', 
                            category = 'cultural')

## Elevation and terrain ruggedness data
elev       <- raster("elev.grd")
elev_below <- raster("elev_below.grd")
tri        <- raster("tri.grd")
tri_below  <- raster("tri_below.grd")

slope <- terrain(elev_below, 
                 opt='slope', 
                 unit='radians', 
                 neighbors=8)

## Projecting the data 
countries10  <- spTransform(countries10, newcrs)
countries110 <- spTransform(countries110, newcrs)
coastline10  <- spTransform(coastline10, newcrs)
ports        <- spTransform(ports, newcrs)



################################################################################
######################### Generating dataset ###################################
################################################################################

## Removing small countries
small <- c("Vanatu", "San Marino", "Vatican", "Fiji", "Solomon Islands",
           "Federated States of Micronesia", "Palau", "Samoa", "Nauru")

study_area <- countries10[!(countries10$SOVEREIGNT %in% small),]


## Make a random sample of points on the coast
sample <- spsample(coastline10, 
                   5000, 
                   type = "random", 
                   method="Line")
sample <- sample[sample(length(sample)),] 
sample <- sample[1:1514]


## Extracting values

## Port locations

# Buffer around each point
a <- gBuffer(ports, width = bwidth, byid=TRUE)

# Elevation in buffer
b <- raster::extract(elev, a)
tri_port <- raster::extract(elev, a)

# Min. elevation
min_elev_port <- sapply(1:length(b), 
                        function(x) min(b[[x]],
                                        na.rm = TRUE))

# Max elevation
max_elev_port <- sapply(1:length(b), 
                        function(x) max(b[[x]],
                                        na.rm = TRUE))

# Mean election
mean_elev_port <- sapply(1:length(b), 
                           function(x) mean(b[[x]],
                                            na.rm = TRUE))

# Mean terrain ruggedness
mean_tri_port <- sapply(1:length(tri_port), 
                         function(x) mean(tri_port[[x]],
                                          na.rm = TRUE))

# Mean slope
mean_slope_port <- sapply(1:length(slope_port), 
                        function(x) mean(tri_port[[x]],
                                         na.rm = TRUE))

## Slope at the port
slope_port  <- raster::extract(slope, ports)
y           <- rep(1, length(slope_port)[1])

port_df <- cbind(slope_port, 
                 elev_port, 
                 min_elev_port, 
                 max_elev_port, 
                 mean_tri_port, 
                 mean_elev_port,
                 y)

## Sampled locations
# Buffer around each point
a <- gBuffer(sample, width = bwidth, byid=TRUE)

# Elevation in buffer
b <- raster::extract(elev_below, a)
tri_port <- raster::extract(elev, a)

# Min.elevation
min_elev_sample <- sapply(1:length(b), 
                          function(x) min(b[[x]], 
                                          na.rm = TRUE))

# Max. Min.elevation
max_elev_sample <- sapply(1:length(b), 
                          function(x) max(b[[x]], 
                                          na.rm = TRUE))

# Mean elevation
mean_elev_sample <- sapply(1:length(b), 
                          function(x) mean(b[[x]], 
                                           na.rm = TRUE))

# Mean terrain ruggedness
mean_tri_sample <- sapply(1:length(tri_port), 
                           function(x) mean(tri_port[[x]], 
                                            na.rm = TRUE))

slope_sample <- raster::extract(slope, sample)
y <- rep(0, length(slope_sample)[1])

no_port_df <- cbind(slope_sample, 
                    mean_tri_sample, 
                    min_elev_sample,
                    max_elev_sample, 
                    mean_elev_sample,
                    y)





## Generating dataframe for prediction
pred_dataframe <- rbind(port_df, no_port_df) %>% 
  data.frame() %>%
  # filter(!is.na(slope_port)) %>% 
  mutate(y=as.factor(y)) %>% 
  rename(slope=slope_port,
         aspect=aspect_port,
         mean_tri=mean_tri_port,
         min_elev=min_elev_port, 
         max_elev=max_elev_port,
         mean_elev=mean_elev_port) %>% 
  mutate(slope2=slope^2,
         aspect2=aspect^2)

pred_dataframe <- pred_dataframe[sample(nrow(pred_dataframe)),] 
# pred_dataframe <-pred_dataframe %>% select(-elev_port)

pred_dataframe_1 <- pred_dataframe %>% dplyr::select(-elev_port) %>% 
  filter(!is.na(mean_elev))












