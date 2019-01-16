## Loading packages and datasets 
set.seed(11042018)

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis, 
       tictoc)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")

# Ports 
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[ports$HARBORTYPE=="CN" & !is.na(ports$HARBORTYPE),]

# Coastline
coastline10 <- ne_download(scale = 10, 
                           type = 'coastline', category = 'physical')

# Countries
countries10 <- ne_download(scale = 10, 
                           type = 'countries', category = 'cultural')

# Elevation data
elev <- raster("ETOPO1_Ice_g_geotiff.tif")

crs(elev) <- crs(countries10)

elev <- projectRaster(elev, crs = newcrs)
countries10 <- spTransform(countries10, newcrs)
coastline10 <- spTransform(coastline10, newcrs)
ports <- spTransform(ports, newcrs)

tic()
  
#study_area <- countries10[countries10$CONTINENT!="Antarctica",]
#study_area <- countries10[countries10$SOVEREIGNT=="Sweden",]
#study_area <- countries10

ports_study_area <- ports
buffer <- gBuffer(study_area, width = 15)
coastline_study_area <- gIntersection(buffer, coastline10) 

#Note: dx denotes the spacing of two horizontaly adjacent points

hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
hexagons <- sapply(1:nrow(hex_points@coords), 
                   function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

## Generates the coast line dataset 
make_raster <- function(x){
  hexagon_cropped = crop(elev, x)
  hexagon_masked = values(mask(hexagon_cropped, x))
}

coast_log <- rep(NA, nrow(hex_points@coords))
for (i in 1:nrow(hex_points@coords)){
  coast_log[i] <- gIntersects(coastline_study_area, hexagons[i])==TRUE
  print(c(i/nrow(hex_points@coords), i))
}
coast_hexagons <- hexagons[coast_log]
# coast_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), 
#function(x) gIntersects(coastline_study_area, hexagons[x]))==TRUE]

# only keep the shortest distance
coast_data <- matrix(nrow=length(coast_hexagons@polygons), ncol = 288)
for (i in 1:length(coast_hexagons@polygons)){
  coast_data[i,] <- make_raster(coast_hexagons[i])[1:288]
  print(c(i/length(coast_hexagons@polygons), i))
}

#coast_data <- t(sapply(1:length(coast_hexagons@polygons), 
#function(x) make_raster(coast_hexagons[x])))
  
y <- as.matrix(sapply(1:length(coast_hexagons@polygons), 
                      function(x) ifelse((gIntersects(ports_study_area, coast_hexagons[x])), 1, 0)))
ID <- sapply(coast_hexagons@polygons, function(x) x@ID)
coast_data_final <- cbind(ID, y, coast_data)
  
## Generates the inland dataset 
# her kan man bruke info fra koden over
inland_hexagons <- hexagons[!coast_log]
#inland_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), 
#function(x) gIntersects(coastline_study_area,hexagons[x]))==FALSE]
inland_data <- sapply(1:length(inland_hexagons@polygons), 
                      function(x) rep(1, dim(coast_data)[2]))
inland_data <- t(inland_data)
y <- as.matrix(rep(0, length(inland_hexagons@polygons)))
ID <- sapply(inland_hexagons@polygons, function(x) x@ID)
inland_data_final <- cbind(ID, y, inland_data)
  
## Making final dataset for prediction
df <- rbind(coast_data_final)
data <- data.frame(df) %>% rename(y = V2) 
#%>% dplyr::select(V462, everything()) 
row.names(data) <- data$ID
data_pred <- subset(data, select = -ID)
  
## Joining the inland and coast line data
df <- rbind(coast_data_final, inland_data_final)
data <- data.frame(df) %>% rename(y = V2) 
#%>% dplyr::select(V462, everything()) 
row.names(data) <- data$ID
data <- subset(data, select = -ID)

toc()

#save.image(file = "output/my_work_space6.RData")






