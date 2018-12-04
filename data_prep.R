
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
elev <- raster("data/ETOPO1_Ice_g_geotiff.tif")
#crs(elev) <- crs(countries10)

pop_density <- raster("data/population_density/gpw_v4_une_atotpopbt_dens_2pt5_min.nc")
crs(pop_density) <- crs(iceland)

countries10 <- spTransform(countries10, newcrs)
coastline10 <- spTransform(coastline10, newcrs)
ports <- spTransform(ports, newcrs)

# Subsetting to region of interest
iceland <- countries10[countries10$ADMIN=="Iceland",]
ports_iceland <- ports[ports$COUNTRY=="IC",]

# Creating a buffer around the region of interest
buffer <- gBuffer(iceland, width = 15)

coastline_iceland <- gIntersection(buffer, coastline10) 
buffer_coastline <- gBuffer(coastline_iceland, width = 30)

#elev_crop <- crop(elev, buffer)


############################
## Generating the dataset ##
############################

y <- c()
hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

df <- data.frame(matrix(ncol = 1050, nrow = 0))

for (i in 1:nrow(hex_points@coords)){

  hexagon <- hexagons[i] 
  
  ## Generate explanatory variable 
  intersection <- gIntersection(iceland, hexagon)
  overlap <- bind(hexagon, intersection)
  ext <- extent(overlap)
  rr <- raster(ext, res=1)
  rr <- rasterize(overlap, rr)
  
  rm <- raster::as.matrix(rr)
  rm[is.na(rm)] = 0
  xi <- as.vector(t(rm))
  
  df[i,] <- xi
  
  ## This loop checks if a polygon overlaps a polygon
  if(gIntersects(ports_iceland, hexagon)){
    y[i] <- 1
  } else{
    y[i] <- 0
  }
}

# Final dataframe for prediction
df[,dim(df)[2]+1] <- y
df <- df[, colSums(df != 0) > 0]
data <- as.tibble(df)
data <- data %>% dplyr::select(V1051, everything()) %>% rename(y = V1051)


############################################
## Predicting using a random forest model ##
############################################

library(ranger)
library(Metrics)
fmodel <- ranger(formula=y~., data=df, num.trees = 100, mtry = 4)

prediction <- predict(fmodel, df)$predictions
y_pred <- ifelse(predict(fmodel, df)$predictions>0.5, 1, 0)

eval_df <- data.frame(y, y_pred, prediction)


###########################
## Joining the dataframe ##
###########################

hexagons <- gIntersection(hexagons, iceland, byid = TRUE)
ID <- sapply(hexagons@polygons, function(x) x@ID)
row.names(eval_df) <- ID

sps_df <- SpatialPolygonsDataFrame(hexagons, eval_df, match.ID = TRUE)


#######################
## Plotting the data ##
#######################

tm_shape(sps_df) +
  tm_fill(col="prediction", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") +
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1)  

# Now take the average of some underlying rasters over the polygons.
# test for other countries
 


## Projecting a raster


tm_shape(pop_density) + tm_raster() + 
  tm_shape(coastline10) + tm_lines()


intersection <- crop(pop_density, iceland)

tm_shape(intersection) + tm_raster(n=100) + 
  tm_shape(iceland) + tm_borders()













# make the buffer out of the coastline border, to make it go faster, then don't need to rin over 
# all the hexagons.

















hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

make_raster <- function(x){
  intersection <- gIntersection(iceland, x)
  overlap <- bind(x, intersection)
  ext <- extent(overlap)
  rr <- raster(ext, res=1)
  rr <- rasterize(overlap, rr)
}
rasters <- sapply(1:length(hexagons@polygons), function(x) make_raster(hexagons[x]))

# Use this chunk instead of the loop, but keep the loop that sets up the dataset, 
# it's still easier to read.







hexagons@polygons[[3]]@Polygons[[1]]@coords
#sapply(hexagons@polygons, function(x) x@Polygons[[1]]@coords)
sapply(hexagons@polygons, function(x) x)


hexagon <- HexPoints2SpatialPolygons(point, dx = 30)
hexagons[i] <- hexagon



make_hexagons <- function(x){
  HexPoints2SpatialPolygons(x, dx = 30)
}







# Vil jeg ha en spatial dataframe sa man kan plotte resulatene ogsa.
# Hvordan regner man gjennomsnittet per shape?
# How much does a coastline look like a natural harbor?



hex_points
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 30)

ports_intersection <- over(hex_grid, ports_iceland)






library(ranger)
library(Metrics)
fmodel <- ranger(formula=y~., data=df, num.trees = 100, mtry = 4)

prediction <- predict(fmodel, df)$predictions
y_pred <- ifelse(predict(fmodel, df)$predictions>0.5, 1, 0)

eval_df <- data.frame(y, y_pred, prediction)































# sa ma den lagres som en raster.

tm_shape(hex_grid) +
  tm_borders(col = "grey")+
  tm_shape(x) +
  tm_fill(col = "grey") + tm_layout(frame=FALSE)

sh <- bind(hex_grid1, x)
ext <- extent(sh)
rr <- raster(ext, res=1)
rr <- rasterize(sh, rr)






# transform to raster, then matrix, then to a tibble of datasets like in the number analysis.
ext <- extent(sh)
rr <- raster(ext, res=1)
rr <- rasterize(sh, rr)

tm_shape(rr)+tm_raster()+ tm_layout(frame=FALSE) 
  

rm <- raster::as.matrix(rr)
rm[is.na(rm)] = 0

x1 <- as.vector(t(rm))

length(x1)

tib <- as.tibble(x1)


# Hver rad er en hexagon



m=matrix(1:12,3,4)
as.vector(m)
as.vector(t(m))

df <- data.frame(matrix(ncol = length(x1), nrow = 0))
df[1,] <- x1


# hvordan kan man finne hvilken som har en nh og ikke?






A = matrix(c(2, 4, 3, 1, 5, 7), c(1,2,3,4,5,6),  nrow=2, ncol=3)          


df1 <- data.frame(a=c(1,2,3), b=c(1,2,3))
df1[,3] <- c(0,0,0)

df2 <- df1[, colSums(df1 != 0) > 0]



tm_shape(iceland) +
  tm_fill() +
  tm_shape(hex_grid) +
  tm_borders(col = "white")+
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1) + tm_layout(frame=FALSE)


overlap <- bind(hex_grid, iceland)



#bruk en buffer

x <- crop(polydf2, polydf1)


buffer


hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30, offset = c(0, 0))
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 30)
plot(study_area, col = "grey50", bg = "light blue", axes = TRUE)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)



## Dataframe with one row for each grid
hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 30)

p1 <- hex_points[20]
hex_grid1 <- HexPoints2SpatialPolygons(p1, dx = 30)
x <- gIntersection(iceland, hex_grid1)




# Find the overlap between 
# Trenger na bare a fargelgge ports, sa klippe det til for 
# presentasjon
g <- gIntersection(hex_grid, iceland, byid = TRUE)

# kan man joine hexagons til en polygon? sa legge til en dataframe? sjekk hva som skjer med id 
# nar man gjor det.

hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)

hexagon1 <- HexPoints2SpatialPolygons(hex_points[1], dx = 30)
hexagon2 <- HexPoints2SpatialPolygons(hex_points[2], dx = 30)
hexagon3 <- HexPoints2SpatialPolygons(hex_points[3], dx = 30)

# Joining two shapefiles

hexagons <- list(1,2,3)


hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30, offset = c(0, 0))
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 30)










## Backup, denne fungerer

## Loading packages
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis)

source("make_grid.R")

newcrs <- CRS("+proj=robin +datum=WGS84 +units=km")

# Ports shapefile
#ports <- st_read("data/WPI_Shapefiles/WPI_Shapefile2010/WPI.shp") %>% 
#  filter(HARBORTYPE=="CN") 

ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[ports$HARBORTYPE=="CN" & !is.na(ports$HARBORTYPE),]

# Coastline
coastline10 <- ne_download(scale = 10, type = 'coastline', category = 'physical')

# Countries
countries10 <- ne_download(scale = 10, type = 'countries', category = 'cultural')

# Elevation data
elev <- raster("data/ETOPO1_Ice_g_geotiff.tif")
crs(elev) <- crs(countries10)

countries10 <- spTransform(countries10, newcrs)
coastline10 <- spTransform(coastline10, newcrs)
ports <- spTransform(ports, newcrs)

# Subsetting to north and south america and creating the grid

iceland <- countries10[countries10$ADMIN=="Iceland",]
ports_iceland <- ports[ports$COUNTRY=="IC",]

buffer <- gBuffer(iceland, width = 15)

coastline_iceland <- gIntersection(buffer, coastline10) 
#elev_crop <- crop(elev, buffer)

## Generate a dataset for each 
y <- c()
hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
df <- data.frame(matrix(ncol = 1050, nrow = 0))
hexagons <- list()

for (i in 1:nrow(hex_points@coords)){
  
  point <- hex_points[i]
  
  # Generate a hexagon around point i
  hexagon <- HexPoints2SpatialPolygons(point, dx = 30)
  hexagons[i] <- hexagon
  
  ## Generate explanatory variable 
  intersection <- gIntersection(iceland, hexagon)
  overlap <- bind(hexagon, intersection)
  ext <- extent(overlap)
  rr <- raster(ext, res=1)
  rr <- rasterize(overlap, rr)
  
  rm <- raster::as.matrix(rr)
  rm[is.na(rm)] = 0
  xi <- as.vector(t(rm))
  
  df[i,] <- xi
  
  ## This loop checks if a polygon overlaps a polygon
  if(gIntersects(ports_iceland, hexagon)){
    y[i] <- 1
  } else{
    y[i] <- 0
  }
}

# Final dataframe for prediction
df[,dim(df)[2]+1] <- y
df <- df[, colSums(df != 0) > 0]
data <- as.tibble(df)
data <- data %>% dplyr::select(V1051, everything()) %>% rename(y = V1051)

# Make a spatial dataframe with the ports and predicted ports
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

library(ranger)
library(Metrics)
fmodel <- ranger(formula=y~., data=df, num.trees = 100, mtry = 4)

prediction <- predict(fmodel, df)$predictions
y_pred <- ifelse(predict(fmodel, df)$predictions>0.5, 1, 0)

eval_df <- data.frame(y, y_pred, prediction)

hexagons <- gIntersection(hexagons, iceland, byid = TRUE)
ID <- sapply(hexagons@polygons, function(x) x@ID)
row.names(eval_df) <- ID

# Joining the dataframe with a spatial object
sps_df <- SpatialPolygonsDataFrame(hexagons, eval_df, match.ID = TRUE)

tm_shape(sps_df) +
  tm_fill(col="prediction", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") +
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1)  






