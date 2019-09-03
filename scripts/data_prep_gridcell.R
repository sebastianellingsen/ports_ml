## This file adds grid cell level data to the main dataset

## loading data

crs_south_america <- "+proj=moll +datum=WGS84 +units=km +pm=bogota"

# Weather and climate data
bio = getData('worldclim', var='bio', res=2.5, lon=22.440823, lat=5.539446)

for(i in 1:20) { 
  
  k <- projectRaster(aggregate(raster(bio, layer=i), 2), 
                     crs = crs_south_america, 
                     method = "bilinear")
  assign(paste("bio", i, sep = "") , k)
  print(i)
}

# Crop suitability 
crop <- c("banana", "coffee", "tobacco", "cotton", "wheat", "tea") 
for(i in crop) { 
  
  i2 <- substr(i, 1, 3)
  path1 <- paste("data/crops", i, sep="/")
  path2 <- paste(paste("res03_crav6190l_silr", i2, sep="_"), ".tif", sep="")
  
  assign(i, projectRaster(raster(paste(path1, path2, sep="/")), 
                          crs = crs_south_america, 
                          method = "bilinear"))
}

# Other raster files
# Sugarcane  
sugarcane <- raster("data/crops/sugar_cane/res03_crav6190l_silr_suc.tif")
sugarcane <- projectRaster(sugarcane, 
                           crs    = crs_south_america, 
                           method = "bilinear")

# Cacao  
cacao <- raster("data/crops/cacao/res03_crav6190l_silr_coc.tif")
cacao <- projectRaster(cacao, 
                       crs    = crs_south_america, 
                       method = "bilinear")

# Maize  
maize <- raster("data/crops/maize/res03_crav6190l_silr_mze.tif")
maize <- projectRaster(maize, 
                       crs    = crs_south_america, 
                       method = "bilinear")


# locations of mineral deposits
mines <- readOGR("data/mines/ofr20051294/ofr20051294.shp", 
                 "ofr20051294")
mines <- spTransform(mines, crs_south_america)

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

# Terrain ruggedness index from Riley et al (1999)
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

rasters  <- c(bio1,    bio2,   bio3,  bio4,  bio5,  bio6,   bio7,  
              bio8,    bio9,   bio10, bio11, bio12, bio13,  bio14,  
              bio15,   bio16,  bio17, bio18, bio19, bio20,  night_lights, 
              elev,    tri,    slope, banana,coffee,tobacco, 
              cotton,  wheat,  tea,   sugarcane,    cacao,   maize)

# Function to extract the raster values for each grid cell
extracting_raster_info <- function(x){
  
  cell             <- south_america[south_america@data$ID==x,]
  cell_values      <- sapply(rasters, 
                             function(raster) raster::extract(raster, cell))
  return(sapply(cell_values, function(x) mean(x, na.rm=TRUE)))

}

# Adding the information from the raster to dataframe
values          <- lapply(south_america@data$ID, 
                         function(x) extracting_raster_info(x))
controls        <- as.data.frame(do.call(rbind, values))
names(controls) <- c("bio1",    "bio2",   "bio3",  "bio4",  "bio5",  "bio6",   "bio7",  
                     "bio8",    "bio9",   "bio10", "bio11", "bio12", "bio13",  "bio14",  
                     "bio15",   "bio16",  "bio17", "bio18", "bio19", "bio20",  "night_lights", 
                     "elev",    "tri",    "slope", "banana","coffee","tobacco", 
                     "cotton",  "wheat",  "tea",   "sugarcane",    "cacao",   "maize")

# Adding the dataframe to the grid 
south_america@data <- cbind(south_america@data, controls)


## Imputing missing values 

# Finding the missing cells  
missing_rows <- south_america[apply(is.na(south_america@data), 1, any),]$ID

# This function takes the nearest cells of the missing cell and imputed the 
# control with a simple average 

neighbors_imputation <- function(x){
  
  # Accessing the cell with missing data 
  cell <- south_america[south_america@data$ID==x,]
  
  # Calculating the distance to other cells
  gd <-  gDistance(south_america,
                   cell,
                   byid=TRUE)
  
  # Finding the closest cells 
  distances1 <- cbind("ID"=colnames(gd), "distance"=gd[1:dim(gd)[2]]) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    mutate(distance=as.numeric(distance)) %>% 
    arrange(distance) %>% 
    top_n(-7) %>% 
    filter(ID!=rownames(gd))
  
  # Neighboring cells 
  neighbors <- south_america[south_america@data$ID %in% distances1$ID, ]
  
  # Imputing the missing values
  missing_rows_number <- which(south_america@data$ID==x)
  missing_variable    <- which(is.na(cell@data)) 
  data_for_imputation <- neighbors@data[ ,missing_variable] %>% as.data.frame()
  imputed_value       <- apply(data_for_imputation, 2, mean, na.rm=TRUE) 
  
  return(cbind(missing_rows_number, missing_variable, imputed_value))
}

# Extracting a list of imputed values
imputed_list <- lapply(missing_rows,
                       function(x) neighbors_imputation(x))

# Adding imputed values to the missing cells 
for(i in 1:length(imputed_list)){
  missing_rows_number <- imputed_list[[i]][1]
  missing_variable <- imputed_list[[i]][,2]
  imputed_value <- imputed_list[[i]][,3]
  
  south_america@data[missing_rows_number, missing_variable] <- imputed_value
}

## Distance to the coastline 
coastline110 <- ne_download(scale = 110, 
                            type = 'coastline', 
                            category = 'physical')

study_area_unprojected_buffer <- gBuffer(study_area_unprojected, 
                                         width = 30)
study_area_coastline          <-gIntersection(coastline110, 
                                              study_area_unprojected_buffer)
study_area_coastline          <- spTransform(study_area_coastline, 
                                             crs_south_america)
gd <-  gDistance(south_america,
                 study_area_coastline,
                 byid=TRUE)

coast_ds <- cbind("ID"=colnames(gd), "distance"=gd[1:dim(gd)[2]]) %>% 
            as.data.frame(stringsAsFactors = FALSE) %>% 
            mutate(distance=as.numeric(distance)) 

# Add distances to the grid cell data  
south_america@data$coast_ds <- coast_ds$distance


## Adding longitude and latitude 
south_america@data$long <- coordinates(south_america)[,1]
south_america@data$lat <- coordinates(south_america)[,2]



## Ports 
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[!is.na(ports$HARBORTYPE),]

# Coastal natural 
ports_cn <- ports[ports$HARBORTYPE=="CN",]

# Projection centered on south america 
ports    <- spTransform(ports, crs_south_america)
ports_cn <- spTransform(ports_cn, crs_south_america)

pports <- readOGR("output/predicted_ports.shp", "predicted_ports")
pports <- spTransform(pports, crs(south_america))


port_probability <- function(x){
  cell <- south_america[south_america@data$ID==x,]
  list <- over(cell, pports, returnList = TRUE)
  return(mean(list[[1]][["prdct"]], na.rm=TRUE))
  
}

south_america@data$port_p <- sapply(south_america@data$ID, 
                                    function(x) port_probability(x))


# Adding distance of the ports and predicted port
distance_to_ports <- function(x){
  cell <- south_america[x,]
  return(gDistance(ports_cn, cell))
}

south_america@data$dis_port <- sapply(1:length(south_america@data$ID), 
                                      function(x) distance_to_ports(x)) 

distance_to_pports <- function(x){
  cell <- south_america[x,]
  return(gDistance(pports, cell))
}

south_america@data$dis_pport <- sapply(1:length(south_america@data$ID), 
                                       function(x) distance_to_pports(x)) 

## Aggregating other data by grid-cell
## Population density 
source("scripts/merge_region_data.R")
south_america@data$popcold <- sapply(south_america@data$ID, 
                                     function(x) 
                                       over(south_america[south_america@data$ID==x,], 
                                            states_sp)$popcold)

## Cities from Chandler 
source("scripts/data_prep_cities.R")
south_america@data$pr_cities <- sapply(south_america@data$ID, 
                                       function(x) 
                                         ifelse(!is.na(over(south_america[south_america@data$ID==x,], 
                                                         cities_sa_pre_hispanic)$City),1,0))

## Archeological sites
source("scripts/data_prep_archeological_sites.R")
south_america@data$sites <- sapply(south_america@data$ID, 
                                   function(x) 
                                     ifelse(!is.na(over(south_america[south_america@data$ID==x,], 
                                                        arch_sites)), 1, 0))


# Reliance on fishing 
source("scripts/data_prep_ethnographic.R")
south_america@data$fish <- sapply(south_america@data$ID, 
                                  function(x) 
                                    over(south_america[south_america@data$ID==x,], 
                                         ethnographic)$v3)

# Sailing times
source("scripts/data_prep_sailing_times.R")
source("data/cliwoc/data_prep_cliwoc.R")

south_america@data$least_costs <- sapply(1:length(south_america@data$ID), 
                                         function(x) sailing_time(x)) 

south_america@data$sailing_time <- ifelse(south_america@data$least_costs>0, 
                                          predict(m1, south_america@data), NA)


