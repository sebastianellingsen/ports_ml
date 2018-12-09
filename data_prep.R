
###################################
## Loading packages and datasets ##
###################################

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis)

source("make_grid.R")

newcrs <- CRS("+proj=robin +datum=WGS84 +units=km")

# Ports shapefile
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[ports$HARBORTYPE=="CN" & !is.na(ports$HARBORTYPE),]

# Coastline
coastline10 <- ne_download(scale = 10, type = 'coastline', category = 'physical')

# Countries
countries10 <- ne_download(scale = 10, type = 'countries', category = 'cultural')

# Elevation data
#elev <- raster("data/ETOPO1_Ice_g_geotiff.tif")
#crs(elev) <- crs(countries10)

pop_density <- raster("data/population_density/gpw_v4_une_atotpopbt_dens_2pt5_min.nc")
#crs(pop_density) <- crs(iceland)

countries10 <- spTransform(countries10, newcrs)
coastline10 <- spTransform(coastline10, newcrs)
ports <- spTransform(ports, newcrs)


generating_data <- function(){
  
  ## Note: The function affects the global environment by 
  ## storing dataframes and sp objects in the workspace.
  
  # Subsetting to region of interest
  #iceland <- countries10[countries10$CONTINENT!="Antarctica" & countries10$CONTINENT!="Seven seas (open ocean)",]
  #ports_iceland <- ports[ports$COUNTRY==port,]
  iceland <- countries10[countries10$ADMIN=="Iceland",]
  ports_iceland <- ports[ports$COUNTRY=="IC",]
  
  # Creating a buffer around the region of interest
  buffer <- gBuffer(iceland, width = 15)

  coastline_iceland <- gIntersection(buffer, coastline10) 
  #buffer_coastline <- gBuffer(coastline_iceland, width = 30)

  #elev_crop <- crop(elev, buffer)

  ## Generating the dataset ##

  
  hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
  hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
  hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
   flatten() %>% 
   do.call(rbind, .)
  
  y <- rep(NA, dim(hex_points@coords)[1])
  df <- data.frame(matrix(ncol = 1050, nrow = dim(hex_points@coords)[1]))

  for (i in 1:nrow(hex_points@coords)){
    hexagon <- hexagons[i] 
    
  
    if(gIntersects(coastline_iceland, hexagons[i])){
    
      ## Generate explanatory variable 
      #intersection <- gIntersection(iceland, hexagon)
      #overlap <- bind(hexagon, intersection)
      #rr <- raster(extent(overlap), res=1)
      #rr <- rasterize(overlap, rr)
      
      #rm <- raster::as.matrix(rr)
      #rm[is.na(rm)] = 0
      #xi <- as.vector(t(rm))
      
      #df[i,] <- xi
      
    
    } else{
      df[i,] <- "inland"
    }
  
   ## This loop checks if a polygon overlaps a polygon
   if(gIntersects(ports_iceland, hexagons[i])){
      y[i] <- 1
    } else{
      y[i] <- 0
   }
  
   print(i/dim(hex_points@coords)[1])
  }
  
  # Final dataframe for prediction
  df[,dim(df)[2]+1] <- y
  df <- df[, colSums(df != 0) > 0]
  data <- as.tibble(df)
  data <- data %>% dplyr::select(V1051, everything()) %>% rename(y = V1051)
  
  dataset <<- data
  hexagons <<- hexagons
  iceland <<- iceland
  ports_iceland <<- ports_iceland
  
}






library(tictoc)
tic()

iceland <- countries10[countries10$ADMIN=="Iceland",]
ports_iceland <- ports[ports$COUNTRY=="IC",]

buffer <- gBuffer(iceland, width = 20)

coastline_iceland <- gIntersection(buffer, coastline10) 
hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

## Generates the coast line dataset 
make_raster <- function(x){
  intersection <- gIntersection(iceland, x)
  overlap <- bind(x, intersection)
  rr <- raster(extent(overlap), res=1)
  rr <- rasterize(overlap, rr)
  rm <- raster::as.matrix(rr)
  rm[is.na(rm)] = 0
  as.vector(t(rm))
}

coast_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_iceland,hexagons[x]))==TRUE]
coast_data <- t(sapply(1:length(coast_hexagons@polygons), function(x) make_raster(coast_hexagons[x])))
y <- as.matrix(sapply(1:length(coast_hexagons@polygons), function(x) ifelse((gIntersects(ports_iceland,coast_hexagons[x])), 1, 0)))
ID <- sapply(coast_hexagons@polygons, function(x) x@ID)
coast_data_final <- cbind(ID, coast_data, y)

## Generates the inland dataset 
inland_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_iceland,hexagons[x]))==FALSE]
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

toc()



hexagons <- gIntersection(hexagons, iceland, byid = TRUE)
row.names(data) <- paste(row.names(data), " 188", sep="")
cut_data <- data[row.names(data)  %in%  sapply(hexagons@polygons, function(x) x@ID),]
sps_df <- SpatialPolygonsDataFrame(hexagons, cut_data, match.ID = TRUE)












tm_shape(sps_df) +
  tm_fill(col="y", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") +
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1)  
# ma beholde columns siden jeg endrer rekkefolgen her, ma legge til column som ID





sapply(1:nrow(data), f(x) row.names(data))











