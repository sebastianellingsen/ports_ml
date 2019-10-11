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
       caret,
       lfe,
       glmnet,
       broom)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
bwidth <- 3

countries_list <- c("CI", 
                    "PE", 
                    "AR", 
                    "UY",   
                    "EC", 
                    "CO", 
                    "VE", 
                    "PA",
                    "SV", 
                    "HN", 
                    "CR", 
                    "GT", 
                    "MX", 
                    "NI", 
                    "CU", 
                    "DO")

## Loading ports datasets

# Ports WPI
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[!is.na(ports$HARBORTYPE),]
ports <- ports[ports$HARBORTYPE=="CN",]
ports <- ports[!(ports$COUNTRY%in%countries_list),]

ports <- ports@data %>%   
  rename(lat = LATITUDE, lon = LONGITUDE) %>% 
  dplyr::select(lat, lon)

# Historical ports datasets
hist_ports1 <- read_xlsx("data/historical_ports/AncientPorts.xlsx", 
                        sheet="PLACES") %>% 
  rename(lat = LATITUDE, lon = LONGITUDE) %>% 
  dplyr::select(lat, lon) %>% 
  filter(!is.na(lat), !is.na(lon))

hist_ports2 <- read_xlsx("data/historical_ports/AncientPorts.xlsx", 
                        sheet="Potential Harb", skip=2) %>% 
  rename(lat = LATITUDE, lon = LONGITUDE) %>% 
  dplyr::select(lat, lon) %>% 
  filter(!is.na(lat), !is.na(lon))

# Making a spatial dataframe 
ports <- rbind(ports, hist_ports1, hist_ports2)
xy    <- cbind(ports$lon, ports$lat)
ports <- SpatialPoints(coords = xy, 
                       proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


## Coastline
coastline10 <- ne_download(scale = 10, 
                           type = 'coastline', 
                           category = 'physical')

coastline110 <- ne_download(scale = 110, 
                           type = 'coastline', 
                           category = 'physical')

## Countries
countries10 <- ne_download(scale = 10, 
                           type = 'countries', 
                           category = 'cultural')

countries110 <- ne_download(scale = 110, 
                            type = 'countries', 
                            category = 'cultural')

## Rivers
rivers50 <- ne_download(scale = 50, 
                        type = 'rivers_lake_centerlines', 
                        category = 'physical')

## Elevation and terrain ruggedness data
elev <- raster("data/elevation/ETOPO1_Ice_g_geotiff.tif")
crs(elev) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
elev <- projectRaster(elev, 
                      crs    = newcrs, 
                      method = "bilinear")

tri        <- raster("data/prepared_rasters/tri.grd")

slope <- terrain(elev, 
                 opt='slope', 
                 unit='radians', 
                 neighbors=8)

## Projecting the data 
countries10  <- spTransform(countries10, newcrs)
countries110 <- spTransform(countries110, newcrs)
coastline10  <- spTransform(coastline10, newcrs)
coastline110 <- spTransform(coastline110, newcrs)
rivers50     <- spTransform(rivers50, newcrs)
ports        <- spTransform(ports, newcrs)



################################################################################
######################### Generating dataset ###################################
################################################################################

## Removing small countries
# small <- c("Vanatu", 
#            "San Marino", 
#            "Vatican", 
#            "Fiji", 
#            "Solomon Islands",
#            "Federated States of Micronesia", 
#            "Palau", 
#            "Samoa", 
#            "Nauru")

# study_area <- countries10[!(countries10$SOVEREIGNT %in% small),]
study_area <- countries10

## Extracting values

## Port locations

# Buffer around each point
a <- gBuffer(ports, width = bwidth, byid=TRUE)

# Elevation in buffer
b           <- raster::extract(elev, a)
tri_port    <- raster::extract(elev, a)
slope_port  <- raster::extract(slope, a)

# Elevation 
elevation_p <- raster::extract(elev, ports)

# Terrain ruggedness 
tri_p <- raster::extract(tri, ports)

# Slope 
slope_p <- raster::extract(slope, ports)

# Min. elevation
min_elev <- sapply(1:length(b), 
                        function(x) min(b[[x]],
                                        na.rm = TRUE))
# Max. elevation
max_elev <- sapply(1:length(b), 
                        function(x) max(b[[x]],
                                        na.rm = TRUE))

# Mean election
mean_elev <- sapply(1:length(b), 
                           function(x) mean(b[[x]],
                                            na.rm = TRUE))

# Mean terrain ruggedness
mean_tri <- sapply(1:length(tri_port), 
                         function(x) mean(tri_port[[x]],
                                          na.rm = TRUE))

# Mean slope
mean_slope<- sapply(1:length(slope_port), 
                        function(x) mean(slope_port[[x]],
                                         na.rm = TRUE))

## Indicator for port
y <- rep(1, length(slope_port)[1])

# Adding continent
cont <- over(a, countries10)$SUBREGION
cont <- ifelse(is.na(cont), "na", cont)

# Measuring distance to river
river_dist <- sapply(1:length(a), 
                     function(i) gDistance(a[i], rivers50))


## Sampling points on the the coastline
sample_coastline <- spsample(coastline10, 
                             500000, 
                             type = "regular", 
                             method="Line")
sample_coastline <- sample_coastline[sample(length(sample_coastline)),] 

## Calculating the length of the coastline 
coastline_length <- function(x){
  ## Finding the closest on point on the coastline
  gd                <-  gDistance(sample_coastline,
                                  ports[x],
                                  byid=TRUE)
  port_on_coastline <-  sample_coastline[apply(gd, 1, which.min)]
 
  ## Measuring the coastal indentation
  port_on_coastline  <-  gBuffer(port_on_coastline, width = 3)
  return(rgeos::gLength(crop(coastline10, port_on_coastline)))
}

coastline_length <- sapply(1:length(ports), 
                           function(x) coastline_length(x))


## Combining the data 
port_df <- cbind(mean_slope, 
                 mean_tri, 
                 min_elev,
                 max_elev, 
                 mean_elev,
                 elevation_p,
                 tri_p,
                 slope_p,
                 cont,
                 coastline_length,
                 river_dist,
                 y)


sample <- spsample(coastline10, 
                   10000, 
                   type = "random", 
                   method="Line")

# Randomize the order of the points
sample <- sample[sample(length(sample)),]
sample <- sample[1:dim(port_df)[1]]

## Sampled locations 
# Buffer around each point
a <- gBuffer(sample, width = bwidth, byid=TRUE)

# Elevation in buffer
b            <-  raster::extract(elev, a)
tri_sample   <-  raster::extract(elev, a)
slope_sample <-  raster::extract(slope, a)

# Elevation 
elevation_p <- raster::extract(elev, sample)

# Terrain ruggedness 
tri_p <- raster::extract(tri, sample)

# Slope 
slope_p <- raster::extract(slope, sample)


# Min.elevation
min_elev <- sapply(1:length(b), 
                          function(x) min(b[[x]], 
                                          na.rm = TRUE))

# Max. Min.elevation
max_elev <- sapply(1:length(b), 
                          function(x) max(b[[x]], 
                                          na.rm = TRUE))

# Mean elevation
mean_elev <- sapply(1:length(b), 
                          function(x) mean(b[[x]], 
                                           na.rm = TRUE))

# Mean terrain ruggedness
mean_tri <- sapply(1:length(tri_sample), 
                           function(x) mean(tri_sample[[x]], 
                                            na.rm = TRUE))

# Mean slope
mean_slope <- sapply(1:length(slope_sample), 
                          function(x) mean(slope_sample[[x]],
                                           na.rm = TRUE))

y <- rep(0, length(slope_sample)[1])


## Adding continent and distance to closest river 

# Adding continent
cont <- over(a, countries10)$SUBREGION
cont <- ifelse(is.na(cont), "na", cont)

# Measuring distance to river
river_dist <- sapply(1:length(a), 
                     function(i) gDistance(a[i], rivers50))

# Adding information on the coastline length

coastline_length <- sapply(1:length(a), 
                           function(x) coastline_length(x))

## Combining the data 
no_port_df <- cbind(mean_slope, 
                    mean_tri, 
                    min_elev,
                    max_elev, 
                    mean_elev,
                    elevation_p,
                    tri_p,
                    slope_p,
                    cont,
                    coastline_length,
                    river_dist,
                    y)

## Generating dataframe for prediction
pred_dataframe <- rbind(port_df, no_port_df) %>% 
  data.frame() %>%
  mutate(y                        = as.factor(y),
         mean_slope               = as.numeric(as.character(mean_slope)), 
         mean_tri                 = as.numeric(as.character(mean_tri)), 
         min_elev                 = as.numeric(as.character(min_elev)),
         max_elev                 = as.numeric(as.character(max_elev)), 
         max_elev                 = as.numeric(as.character(max_elev)), 
         coastline_length         = as.numeric(as.character(coastline_length)), 
         river_dist               = as.numeric(as.character(river_dist)), 
         mean_elev                = as.numeric(as.character(mean_elev)),
         elevation_p              = as.numeric(as.character(elevation_p)),
         tri_p                    = as.numeric(as.character(tri_p)),
         slope_p                  = as.numeric(as.character(slope_p)))

## Saving file as a csv
pred_dataframe1 <- pred_dataframe[sample(dim(pred_dataframe)[1]), ]
write.csv(pred_dataframe, "output/pred_dataframe.csv", row.names = FALSE)


