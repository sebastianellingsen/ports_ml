## This file adds grid cell level data to the main dataset
library(spatialEco)

## Countries in the main sample 
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
                    "Dominican Republic")

## Weather and climate data
bio = getData('worldclim', var='bio', res=2.5, lon=22.440823, lat=5.539446)

for(i in 1:20) { 
  k <- projectRaster(aggregate(raster(bio, layer=i), 2), 
                     crs = crs(south_america), 
                     method = "bilinear")
  assign(paste("bio", i, sep = "") , k)
  print(i)
}


## Loading files on crop suitability 
file_names <- list.files('data/crops')
load_crops <- function(x){
  
  path_tmp       <- paste('data/crops', file_names[x], sep = '/')
  file_names_tmp <- list.files(path_tmp)
  index <- which(substr(file_names_tmp, 
                        nchar(file_names_tmp)-2, 
                        nchar(file_names_tmp)) == 'tif')
  file_tmp <- file_names_tmp[index]
  raster_tmp     <- raster(paste(path_tmp, file_tmp, sep = '/'))
  assign(file_names[x], raster_tmp)
}
sapply(1:length(file_names), function(x) load_crops(x))


# Loading files on the location of mineral deposits 
mines <- readOGR("data/mines/ofr20051294/ofr20051294.shp", 
                 "ofr20051294")








# Elevation data
elev               <- raster("data/elevation/ETOPO1_Ice_g_geotiff.tif")
crs(elev)          <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
south_america_unprojected <- spTransform(south_america, 
                                         "+proj=longlat +datum=WGS84 +ellps=WGS84")
elev_south_america <- crop(elev, south_america_unprojected)
elev               <- aggregate(elev_south_america, 
                                fact = 4, 
                                fun = mean)
elev               <- projectRaster(elev, 
                                    crs = crs_south_america, 
                                    method = "bilinear")

# Terrain ruggedness index from Riley et al. (1999)
tri <- tri(elev, s = 3, exact = TRUE)

# Slope
slope <- terrain(elev, 
                 opt='slope', 
                 unit='radians', 
                 neighbors=8)

# Night lights
night_lights               <- raster("data/nightlights/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")
night_lights_south_america <- crop(night_lights, 
                                   study_area_unprojected)
night_lights               <- aggregate(night_lights_south_america, 
                                        fact = 6, 
                                        fun = mean)
night_lights               <- projectRaster(night_lights, 
                                            crs = crs_south_america, 
                                            method = "bilinear")

rasters  <- c(bio1,    bio2,   bio3,  bio4,   bio5,   bio6,    bio7,  
              bio8,    bio9,   bio10, bio11,  bio12,  bio13,   bio14,  
              bio15,   bio16,  bio17, bio18,  bio19,  bio20,   night_lights, 
              elev,    tri,    slope, banana, coffee, tobacco, 
              cotton,  wheat,  tea,   sugarcane,    cacao,     maize)

# Function to extract the raster values for each grid cell
extracting_raster_info <- function(x){
  
  cell             <- south_america[south_america@data$ID==x,]
  cell_values      <- sapply(rasters, 
                             function(raster) raster::extract(raster, cell))
  return(sapply(cell_values, function(x) mean(x, na.rm=TRUE)))
  print(x)
}

# Adding the information from the raster to dataframe
values          <- lapply(south_america@data$ID, 
                         function(x) extracting_raster_info(x))
controls        <- as.data.frame(do.call(rbind, values))
names(controls) <- c("bio1",    
                     "bio2",   
                     "bio3",  
                     "bio4",  
                     "bio5",  
                     "bio6",   
                     "bio7",  
                     "bio8",    
                     "bio9",   
                     "bio10", 
                     "bio11", 
                     "bio12", 
                     "bio13",  
                     "bio14",  
                     "bio15",   
                     "bio16",  
                     "bio17", 
                     "bio18", 
                     "bio19", 
                     "bio20",  
                     "night_lights", 
                     "elev",    
                     "tri",    
                     "slope", 
                     "banana",
                     "coffee",
                     "tobacco", 
                     "cotton",  
                     "wheat",  
                     "tea",   
                     "sugarcane",    
                     "cacao",   
                     "maize")

# Adding the dataframe to the grid 
south_america@data <- cbind(south_america@data, controls)


## Distance to the coastline 
coastline110 <- ne_download(scale = 110, 
                            type = 'coastline', 
                            category = 'physical')
countries110 <- ne_download(scale = 110, 
                            type = 'countries', 
                            category = 'cultural')

study_area_unprojected <- countries110[countries110@data$ADMIN %in% countries_list, ]
study_area_unprojected_buffer <- gBuffer(study_area_unprojected, 
                                         width = 30)
study_area_coastline          <-gIntersection(coastline110, 
                                              study_area_unprojected_buffer)
study_area_coastline          <- spTransform(study_area_coastline, 
                                             crs(south_america))
gd <-  gDistance(south_america,
                 study_area_coastline,
                 byid=TRUE)

coast_ds <- cbind("ID"=colnames(gd), "distance"=gd[1:dim(gd)[2]]) %>% 
            as.data.frame(stringsAsFactors = FALSE) %>% 
            mutate(distance=as.numeric(distance)) 

# Add distances to the grid cell data  
south_america@data$coast_ds <- coast_ds$distance

## Adding longitude and latitude 
# coordinates under the projection used
south_america@data$long_proj <- coordinates(south_america)[,1]
south_america@data$lat_proj <- coordinates(south_america)[,2]






## Making virtual countries for fixed effects 
countries110 <- ne_download(scale    = 110, 
                            type     = 'countries', 
                            category = 'cultural')

study_area <- countries110[countries110@data$ADMIN%in%countries_list, ]
study_area <- spTransform(study_area, 
                          crs(south_america))
study_area_buffer <- gBuffer(study_area, width=300)

hex_points <- spsample(study_area_buffer, 
                       type = "hexagonal", 
                       cellsize = 300)

hexagons <- sapply(1:nrow(hex_points@coords), 
                   function(x) HexPoints2SpatialPolygons(hex_points[x],
                                                         dx = 300))

hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)


extracting_vc_country <- function(x){
  cell                <- south_america[south_america@data$ID==x,]
  k <- over(cell, hexagons)[[1]]
  return(over(cell, hexagons))
}

vc_country <- sapply(south_america@data$ID, 
                         function(x) extracting_vc_country(x))

south_america@data$vc_country <- vc_country


## Distance to major cities 
dat = read.csv("data/chandler/chandlerV2.csv",
               stringsAsFactors=FALSE, fileEncoding="latin1")  %>% 
  mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude)) %>% 
  # filter(!is.na(Longitude), !is.na(Latitude)) %>% 
  gather("year", "pop",7:812)  %>% 
  filter(!is.na(pop)) %>% 
  dplyr::select(-OtherName) %>% 
  filter(!is.na(Longitude), !is.na(Latitude)) 

dat$year <- as.numeric(str_replace_all(dat$year, "AD_", ""))
dat <- dat[-c(1:130),]

## Make a spatial dataframe
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coords = cbind(dat$Longitude, dat$Latitude)
sp = SpatialPoints(coords)

cities = SpatialPointsDataFrame(coords, dat)
cities = SpatialPointsDataFrame(sp, dat)
crs(cities)=crs

cities_sa <- cities[cities@data$Country %in% countries_list,]

cities_sa <- spTransform(cities_sa, crs(south_america))
cities_sa <- cities_sa[cities_sa@data$City %in% 
                         c("Mexico City", "Potosi", "Quito", "Lima") &
                         cities_sa@data$year == 1700, ]

dist_capital <- function(city, x){
  city <- cities_sa[cities_sa@data$City == city, ]
  cell <- south_america[south_america@data$ID==x,]
  return(gDistance(cell, city))
}

d_Potosi <- sapply(south_america@data$ID, function(x) dist_capital("Potosi", x))
d_Mx     <- sapply(south_america@data$ID, function(x) dist_capital("Mexico City", x))
d_Quito  <- sapply(south_america@data$ID, function(x) dist_capital("Quito", x))
d_Lima   <- sapply(south_america@data$ID, function(x) dist_capital("Lima", x))

south_america@data$d_Potosi <- d_Potosi
south_america@data$d_Mx     <- d_Mx
south_america@data$d_Quito  <- d_Quito
south_america@data$d_Lima   <- d_Lima


## Distance to mineral deposits
mines <- readOGR('data/mines/ofr20051294/ofr20051294.shp', 'ofr20051294')
mines <- mines[mines@data$COUNTRY %in% countries_list,]
mines <- spTransform(mines, crs(south_america))

south_america@data$mine <- sapply(south_america@data$ID, 
                                   function(x) 
                                     gDistance(south_america[south_america@data$ID==x,], 
                                               mines))

## Distance to lakes and rivers
## Rivers
rivers50 <- ne_download(scale = 50, 
                        type = 'rivers_lake_centerlines', 
                        category = 'physical')
rivers50 <- spTransform(rivers50, crs(south_america))
south_america@data$rivers <- sapply(south_america@data$ID, 
                                   function(x) 
                                     gDistance(south_america[south_america@data$ID==x,], 
                                               rivers50))

## lakes
lakes50 <- ne_download(scale = 50, 
                        type = 'lakes', 
                        category = 'physical')
lakes50 <- spTransform(lakes50, crs(south_america))

south_america@data$lakes <- sapply(south_america@data$ID, 
                                  function(x) 
                                    gDistance(south_america[south_america@data$ID==x,], 
                                              lakes50))


## Distance to contemporary ports 
wpi_countries_list <- c("CL", 
                        "PE", 
                        "AR", 
                        "UY",   
                        "EC",
                        "HO",
                        "CO", 
                        "VE", 
                        "PA",
                        "HN", 
                        "CR",
                        "CS",
                        "GT", 
                        "MX", 
                        "CU", 
                        "DO",
                        "NU",
                        "PM",
                        "ES",
                        "CI",
                        "DR")

ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[!is.na(ports$HARBORTYPE) & ports$COUNTRY %in% wpi_countries_list,]

ports <- ports[!(ports@data$HARBORSIZE %in% c('S', 'V')), ]

# Projection centered on south america
ports    <- spTransform(ports, crs(south_america))
ports_cn <- spTransform(ports_cn, crs(south_america))

distance_to_port <- function(x){
  cell <- south_america[south_america@data$ID==x,]
  return(gDistance(cell, ports))
}
south_america@data$dist_port <- sapply(south_america@data$ID, 
                                       function(x) distance_to_port(x))


## Distance to predicted ports
predicted_ports <- readOGR("output/predicted_ports.shp", "predicted_ports")
predicted_ports <- spTransform(predicted_ports, crs(south_america))
pports <- predicted_ports[predicted_ports@data$predctn>0.85,]

distance_to_pport <- function(x){
  cell <- south_america[south_america@data$ID==x,]
  return(gDistance(cell, pports))
}
south_america@data$dist_pport <- sapply(south_america@data$ID, 
                                          function(x) distance_to_pport(x))

predicted_ports_l <- readOGR("output/predicted_ports_l.shp", "predicted_ports_l")
predicted_ports_l <- spTransform(predicted_ports_l, crs(south_america))
distance_to_pport_l <- function(x){
  cell <- south_america[south_america@data$ID==x,]
  return(gDistance(cell, predicted_ports_l))
}
south_america@data$dist_pport_l <- sapply(south_america@data$ID, 
                                        function(x) distance_to_pport_l(x))

south_america@data$close_pport_l <- ifelse(south_america@data$dist_pport_l<75, 1, 0)


## Distance to ports in 1777
distance_to_port_1777 <- function(x){
  cell <- south_america[south_america@data$ID==x,]
  return(gDistance(cell, ports))
}
south_america@data$dist_port_1777<- sapply(south_america@data$ID, 
                                           function(x) distance_to_port_1777(x))


## Distance to the free ports
free_ports <- ports[ports@data$`Port status`!='Restricted', ]
free_ports <- spTransform(free_ports, crs(south_america))

distance_free_port <- function(x){
  cell <- south_america[south_america@data$ID == x,]
  return(gDistance(free_ports, cell))
}

south_america@data$distance_free_port <- sapply(south_america@data$ID, 
                                                function(x) distance_free_port(x))



## Finding the name of the closest historical port
ports_projected <- spTransform(ports, crs(south_america))
closest_port <- function(x){
  cell <- south_america[south_america@data$ID == x,]
  dist <- which.min(gDistance(ports_projected, cell, byid = TRUE))
  return(ports[dist,]@data$ports)
}
south_america@data$c_port <- sapply(south_america@data$ID, 
                                      function(x) closest_port(x))



# for each cell, calculate the distance to each, 
# return the one that is closest








## Preparing data on roads and railways
source('scripts/data_prep_infrastructure.R')
## Overlapping road
over_road <- function(x){
  cell <- south_america[south_america@data$ID==x,]
  k <- dim(over(cell, roads, returnList = T)[[1]])[1]
  return(ifelse(k>1, 1, 0))
}
south_america@data$road <- sapply(south_america@data$ID, 
                                 function(x) over_road(x))

## Overlapping railroad
over_railroad <- function(x){
  cell <- south_america[south_america@data$ID==x,]
  k <- dim(over(cell, railroads, returnList = T)[[1]])[1]
  return(ifelse(k>1, 1, 0))
}

south_america@data$railroad <- sapply(south_america@data$ID, 
                                        function(x) over_railroad(x))


pop <- raster("data/population_density/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev10_2015_2pt5_min_tif/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev10_2015_2pt5_min.tif")
pop <- projectRaster(pop, 
                     crs = crs_south_america, 
                     method = "bilinear")

## Calculating the max. population density by grid cell 
population_by_cell <- function(x){
      cell <- south_america[south_america@data$ID==x,]
      return(mean(extract(pop, cell) %>% unlist(), na.rm = T))
}
south_america@data$pop <- sapply(south_america@data$ID, 
                                 function(x) population_by_cell(x))


## Estimating the sailing distance along the coast


## Population density 
source("scripts/merge_region_data.R")
south_america@data$popcold <- sapply(south_america@data$ID, 
                                     function(x) 
                                       over(south_america[south_america@data$ID==x,], 
                                            states_sp)$popcold)

## Archeological sites
source("scripts/data_prep_archeological_sites.R")
south_america@data$sites <- sapply(south_america@data$ID, 
                                   function(x) 
                                     ifelse(!is.na(over(south_america[south_america@data$ID==x,], 
                                                        arch_sites)), 1, 0))

south_america@data$sites <- sapply(south_america@data$ID, 
                                   function(x) 
                                     gDistance(south_america[south_america@data$ID==x,], 
                                                        arch_sites))

## Defining variables
south_america@data$urban           <- ifelse(south_america@data$night_lights>0, 1, 0)
south_america@data$coastal         <- ifelse(south_america@data$coast_ds<=75, 1, 0)
south_america@data$close_port      <- ifelse(south_america@data$dist_port<=75, 1, 0)
south_america@data$close_port_1777 <- ifelse(south_america@data$dist_pport_l<=75, 1, 0)





































## loading the population data, is it more consentrated along the coast in some periphery?
df1 <- south_america[south_america@data$ccode=='Mexico',]

ggplot(data = df1@data, aes(x = coast_ds, y = log(1+night_lights))) + 
  geom_point(alpha = 0.1)+
geom_smooth(method='lm', se=F, col='black', size=0.3, show.legend = F)

df1 <- south_america[south_america@data$ccode=='Peru',]

summary(felm(data = df1, 
          log(pop) ~ coast_ds|
          as.factor(states)|
          0|
          states))























