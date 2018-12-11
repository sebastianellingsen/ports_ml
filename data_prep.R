## Loading packages and datasets 

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis, tictoc)

newcrs <- CRS("+proj=robin +datum=WGS84 +units=km")

# Ports 
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[ports$HARBORTYPE=="CN" & !is.na(ports$HARBORTYPE),]

# Coastline
coastline10 <- ne_download(scale = 10, type = 'coastline', category = 'physical')

# Countries
countries10 <- ne_download(scale = 10, type = 'countries', category = 'cultural')

countries10 <- spTransform(countries10, newcrs)
coastline10 <- spTransform(coastline10, newcrs)
ports <- spTransform(ports, newcrs)

## Function generating the main dataset 
generating_data <- function(){
  
  # Note: The function affects the global environment by 
  # storing dataframes and sp objects in the workspace.
  
  tic()
  
  study_area <- countries10[countries10$ADMIN=="Iceland",]
  ports_study_area <- ports[ports$COUNTRY=="IC",]
  buffer <- gBuffer(study_area, width = 15)
  
  coastline_study_area <- gIntersection(buffer, coastline10) 
  hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
  hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
  hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
    flatten() %>% 
    do.call(rbind, .)
  
  ## Generates the coast line dataset 
  make_raster <- function(x){
    intersection <- gIntersection(study_area, x)
    overlap <- bind(x, intersection)
    rr <- raster(extent(overlap), res=1)
    rr <- rasterize(overlap, rr)
    rm <- raster::as.matrix(rr)
    rm[is.na(rm)] = 0
    as.vector(t(rm))
  }
  
  coast_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_study_area, hexagons[x]))==TRUE]
  coast_data <- t(sapply(1:length(coast_hexagons@polygons), function(x) make_raster(coast_hexagons[x])))
  
  y <- as.matrix(sapply(1:length(coast_hexagons@polygons), function(x) ifelse((gIntersects(ports_study_area,coast_hexagons[x])), 1, 0)))
  ID <- sapply(coast_hexagons@polygons, function(x) x@ID)
  coast_data_final <- cbind(ID, coast_data, y)
  
  ## Generates the inland dataset 
  inland_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_study_area,hexagons[x]))==FALSE]
  inland_data <- sapply(1:length(inland_hexagons@polygons), function(x) rep(1, dim(coast_data)[2]))
  inland_data <- t(inland_data)
  y <- as.matrix(rep(0, length(inland_hexagons@polygons)))
  ID <- sapply(inland_hexagons@polygons, function(x) x@ID)
  inland_data_final <- cbind(ID, inland_data, y)
  
  ## Joining the inland and coast line data
  df <- rbind(coast_data_final, inland_data_final)
  data <- data.frame(df) %>% dplyr::select(V1052, everything()) %>% rename(y = V1052) 
  row.names(data) <- data$ID
  data <- subset(data, select = -ID)
  
  dataset <<- data
  hexagons <<- hexagons
  study_area <<- study_area
  ports_study_area <<- ports_study_area
  
  toc()
}

