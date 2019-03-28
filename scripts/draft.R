###################################
## Loading packages and datasets ##
###################################

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis)

source("make_grid.R")

newcrs <- CRS("+proj=robin +datum=WGS84 +units=km")


ports1 <- readOGR("ports_x010g.shp_nt00960/ports_x010g.shp")


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
iceland <- countries10[countries10$ADMIN=="Denmark",]
ports_iceland <- ports[ports$COUNTRY=="IC",]

# Creating a buffer around the region of interest
buffer <- gBuffer(iceland, width = 15)

coastline_iceland <- gIntersection(buffer, coastline10) 
buffer_coastline <- gBuffer(coastline_iceland, width = 20)

#elev_crop <- crop(elev, buffer)


############################
## Generating the dataset ##
############################

# denne ma utvides med en gang ikke etterpa
y <- c()
hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

df <- data.frame(matrix(ncol = 1050, nrow = 0))

for (i in 1:nrow(hex_points@coords)){
  
  hexagon <- hexagons[i] 
  
  if(gIntersects(coastline_iceland, hexagon)){
    
    ## Generate explanatory variable 
    intersection <- gIntersection(iceland, hexagon[1])
    overlap <- bind(hexagon, intersection)
    ext <- extent(overlap)
    rr <- raster(ext, res=1)
    rr <- rasterize(overlap, rr)
    
    rm <- raster::as.matrix(rr)
    rm[is.na(rm)] = 0
    xi <- as.vector(t(rm))
    
    df[i,] <- xi
    
  } else{
    df[i,] <- 1
  }
  
  ## This loop checks if a polygon overlaps a polygon
  if(gIntersects(ports_iceland, hexagon)){
    y[i] <- 1
  } else{
    y[i] <- 0
  }
  
  print(i)
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

# her er det egentlig bare flaks at det fungerer... fikser dette etterhvert

#######################
## Plotting the data ##
#######################


tm_shape(sps_df) +
  tm_fill(col="prediction", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") +
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1)  



# Vet at de som ikke grenser coast bufferen ikke inneholder noen port.
# Kan fjerne disse fra datasettet. Trekk så ut tilfeldige grupper.

## Ikke lagre informasjon om raster hvis den ikke er i kystområde,
##  kan spare mye tid pa den maten


# endringer: as.matrix opplegg, enda en if statement i loopen, 















# Now take the average of some underlying rasters over the polygons.

## Projecting a raster, kan ikke endre raster bare med a assigne crs
## To do: projecte raster, gjore cross validation med mer standard modeller for a se hvor bra
## modellen fungerer.
## (i) ta alle ports (ii) tilfeldig utvalg av andre områder (iii) dette er datasettet man bruker
## til å finne modellen. (iv) Ta så å sammenlikn modellene, gjør dette i R, gjør deep learning i python.
## Når dette sitter kan man gjøre:
## (i) first stage effect (ii) reduced form på outcomes.
## What's the long run impact of harbors within countries and across countries. (average effect of a harbor)

# convolutional neural networks perform very well.

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



a<-matrix(nrow=4,ncol=5)
b<-matrix(nrow=4,ncol=5)
A <- (rbind(a,b))



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

buffer <- gBuffer(iceland, width = 20)

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
fmodel <- ranger(formula=y~., data=data, num.trees = 100, mtry = 4)

prediction <- predict(fmodel, data)$predictions
y_pred <- ifelse(predict(fmodel, data)$predictions>0.5, 1, 0)

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




p <- seq(0,1,0.01)

pred_by_threshold <- map(p, function (threshold) map(cv_eval$predicted, ~ifelse(.x>threshold, 1, 0)))

map_dbl(cv_eval$validate_actual, ~cv_eval$validate_actual)


map2(cv_eval$predicted, ~ifelse(.x>0.5, 1, 0))

map2_dbl(validate_actual, y_pred, ~accuracy(.x,.y)))













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



#for (i in 1:length(coast_hexagons@polygons)){
#  hexagon <- coast_hexagons[618]
#  
#  intersection <- gIntersection(iceland, hexagon)
#  overlap <- bind(hexagon, intersection)
#  rr <- raster(extent(overlap), res=1)
#  rr <- rasterize(overlap, rr)

#  rm <- raster::as.matrix(rr)
#  rm[is.na(rm)] = 0
#  xi <- as.vector(t(rm))
#  print(i)
#}







## Older version of the loading data program


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
  
  paste("dataset", , sep="_")  <<- data
  hexagons <<- hexagons
  iceland <<- iceland
  ports_iceland <<- ports_iceland
  
}




library(tictoc)
tic()

iceland <- countries10[countries10$ADMIN=="Iceland",]
ports_iceland <- ports[ports$COUNTRY=="IC",]
buffer <- gBuffer(iceland, width = 15)

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

coast_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_iceland, hexagons[x]))==TRUE]
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





for (i in 1:length(coast_hexagons@polygons)){
  hexagon <- coast_hexagons[i]
  
  intersection <- gIntersection(study_area, hexagon)
  overlap <- bind(hexagon, intersection)
  rr <- raster(extent(overlap), res=1.5)
  rr <- rasterize(overlap, rr)
  
  rm <- raster::as.matrix(rr)
  rm[is.na(rm)] = 0
  xi <- as.vector(t(rm))
  print(i)
}






## Loading packages and datasets 

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis, tictoc)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")

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


tic()

#study_area <- countries10[countries10$CONTINENT=="South America",]
study_area <- countries10[countries10$ADMIN=="Iceland",]

ports_study_area <- ports
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
  rr <- raster(extent(overlap), res=1.5)
  rr <- rasterize(overlap, rr)
  rm <- raster::as.matrix(rr)
  rm[is.na(rm)] = 0
  as.vector(t(rm))
}

coast_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_study_area, hexagons[x]))==TRUE]

for (i in 1:length(coast_hexagons@polygons)){
  make_raster(coast_hexagons[i])
  print(i)
}

# coast_data <- t(sapply(1:length(coast_hexagons@polygons), function(x) make_raster(coast_hexagons[x])))

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





str_replace_all(x, fixed(" "), "")



#ports_study_area <- ports[ports$COUNTRY=="IC",]



# sa: 43.14088


# fiks kontinentene hver for seg, sett dem sa sammen, ryddigere og enklere a finne hva som 
# ikke fungerer.




cuba <- gIntersection(hexagons_namerica, study_area, byid = TRUE) 










## Loading packages and datasets 
set.seed(11042018)

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis, tictoc)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")

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


tic()

#study_area <- countries10[countries10$CONTINENT=="Asia",]
#study_area <- countries10[countries10$ADMIN=="Sweden",]
study_area <- countries10

ports_study_area <- ports
buffer <- gBuffer(study_area, width = 15)
coastline_study_area <- gIntersection(buffer, coastline10) 

#Note: dx denotes the spacing of two horizontaly adjacent points

hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

## Generates the coast line dataset 
make_raster <- function(x){
  intersection <- gIntersection(study_area, x)
  overlap <- bind(x, intersection)
  rr <- raster(extent(overlap), res=1.5)
  rr <- rasterize(overlap, rr)
  rm <- raster::as.matrix(rr)
  rm[is.na(rm)] = 0
  as.vector(t(rm))
}

coast_log <- rep(NA, length(hex_points@coords))
for (i in 1:nrow(hex_points@coords)){
  coast_log[i] <- gIntersects(coastline_study_area, hexagons[i])==TRUE
  print(c(i/nrow(hex_points@coords), i))
}
coast_hexagons <- hexagons[coast_log]
# coast_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_study_area, hexagons[x]))==TRUE]

coast_data <- matrix(nrow=length(coast_hexagons@polygons), ncol = 460)
for (i in 1:length(coast_hexagons@polygons)){
  coast_data[i,] <- make_raster(coast_hexagons[i])
  print(c(i/length(coast_hexagons@polygons), i))
}

#coast_data <- t(sapply(1:length(coast_hexagons@polygons), function(x) make_raster(coast_hexagons[x])))

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
data <- data.frame(df) %>% dplyr::select(V462, everything()) %>% rename(y = V462) 
row.names(data) <- data$ID
data <- subset(data, select = -ID)

dataset_iceland <<- data
hexagons_iceland <<- hexagons
study_area_iceland <<- study_area
ports_study_area_iceland <<- ports_study_area

toc()

#save.image(file = "output/my_work_space.RData")








#####
#Prediction final 
#####




################################################################
## This file splits data in test and training and fits various
## models on the data. The results are evaluated using 
## cross validation.
################################################################

###############################
## Loading data and packages ##
###############################

#source("data_prep.R")

if (!require(pacman)) install.packages("pacman")
p_load(ranger, Metrics, broom, rsample, tidyverse)

######################
## Model evaluation ##
######################

#dataset = rbind(dataset_samerica, dataset_namerica, dataset_africa, dataset_asia, dataset_europe)
set.seed(11042018)

#save(dataset, file="dataset.Rda")
load("dataset.Rda")

## use the sample function in R
# Generate final dataset used to predict
data <- dataset_samerica[which(!is.na(dataset$V2)),]
data$sample <- rbinom(n=nrow(data),prob=1/20, size=1)
#data <- data %>% filter(y == 1 | (sample == 1 & y!=1)) %>% dplyr::select(-sample)
data <- data %>% filter(y == 1 | (sample == 1)) %>% dplyr::select(-sample)
data$y <- as.numeric(data$y)-1





## Splitting the data to training, testing  
data_split <- initial_split(data, 0.75)
training_data <- training(data_split)
testing_data <- testing(data_split)

## Cross validation folds
cv_split <- vfold_cv(training_data, v = 10)
cv_data <- cv_split %>% 
  mutate(train = map(splits, ~training(.x)), validate = map(splits, ~testing(.x)))

## Fitting the model
cv_models <- cv_data %>% 
  mutate(forest_model = map(train, ~ranger(formula= y~., data=.x, num.trees = 2, mtry = 10)))

cv_prep <- cv_models %>% 
  mutate(validate_actual = map(validate, ~.x$y)) %>% 
  mutate(predicted = map2(forest_model, validate, ~predict(.x,.y)$predictions)) 

## Validating the fit of the model
#dta$prediction <- ifelse(pred>=0.3,1,0)
confusion_matrix <- map2(cv_prep$validate_actual, cv_prep$predicted, ~table(.x,.y))
Reduce(`+`, confusion_matrix)

cv_eval  %>% 
  mutate(error = map2_dbl(validate_actual, y_pred, ~mae(.x,.y))) %>% 
  mutate(precision = map2_dbl(validate_actual, y_pred, ~precision(.x,.y))) %>% 
  mutate(accuracy = map2_dbl(validate_actual, y_pred, ~accuracy(.x,.y)))


er <- map(cv_eval$error, ~mean(.x)) %>% unlist() %>% sort(decreasing=TRUE)
plot(er)

## Testing data
model_testing <- ranger(formula= y~., data=training_data, num.trees = 2, mtry = 10)
prediction <- predict(model_testing, testing_data)$predictions
confusion_matrix <-table(prediction, testing_data$y)










# ROC curves
library(ROCR)




## Fitting the full model to the training data
model <- ranger(formula= y~., data=training_data, num.trees = 30, mtry = 10)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

plot(perf)


x_rf <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf <- y_rf[x_rf<=1]
x_rf <- x_rf[x_rf<=1]



model <- ranger(formula= y~., data=training_data, num.trees = 2, mtry = 10)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

x_rf1 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf1 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf1 <- y_rf1[x_rf1<=1]
x_rf1 <- x_rf1[x_rf1<=1]



model <- ranger(formula= y~., data=training_data, num.trees = 30, mtry = 150)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

x_rf2 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf2 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf2 <- y_rf2[x_rf2<=1]
x_rf2 <- x_rf2[x_rf2<=1]


model <- ranger(formula= y~., data=training_data, num.trees = 50, mtry = 80)

prediction <- predict(model, testing_data)$predictions
pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")
x_rf3 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf3 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf3 <- y_rf3[x_rf3<=1]
x_rf3 <- x_rf3[x_rf3<=1]



ggplot() + 
  geom_line(aes(x_rf, y_rf), color="Blue") + 
  #geom_line(aes(x_rf1, y_rf1), color="Red") +
  #geom_line(aes(x_rf2, y_rf2), color="Red") +
  #geom_line(aes(x_rf3, y_rf3), color="Orange") +
  xlab("False positive rate") + ylab("True positive rate") + ggtitle("Receiver operating characteristic") +
  geom_segment(aes(x = 0, y = 0, xend = 0.98, yend = 1, colour = "segment"),alpha=0.3, linetype = 2) +
  scale_x_continuous(limits = c(0, 1)) + theme_classic() 




# shows the roc of a pruned random forest





###########################
## Joining the dataframe ##
###########################

hexagons_full <- gIntersection(hexagons, iceland, byid = TRUE)

ID <- sapply(hexagons_full@polygons, function(x) x@ID)
row.names(eval_df) <- ID

sps_df <- SpatialPolygonsDataFrame(hexagons_full, eval_df, match.ID = TRUE)


#######################
## Plotting the data ##
#######################

tm_shape(sps_df) +
  tm_fill(col="prediction", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons_full) + tm_borders(col = "white") +
  tm_shape(ports_iceland) + tm_dots(size=0.1) 


hexagons <- gIntersection(hexagons, study_area, byid = TRUE)
row.names(dataset) <- paste(row.names(dataset), " 188", sep="")
cut_data <- dataset[row.names(dataset)  %in%  sapply(hexagons@polygons, function(x) x@ID),]
sps_df <- SpatialPolygonsDataFrame(hexagons, cut_data, match.ID = TRUE)

tm_shape(sps_df) +
  tm_fill(col="y") + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") 

+
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1)  




## Loading packages and datasets 
set.seed(11042018)

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, viridis, tictoc)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")

# Ports 
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[ports$HARBORTYPE=="CN" & !is.na(ports$HARBORTYPE),]

# Coastline
coastline10 <- ne_download(scale = 10, type = 'coastline', category = 'physical')

# Countries
countries10 <- ne_download(scale = 10, type = 'countries', category = 'cultural')

# Elevation data
elev <- raster("data/ETOPO1_Ice_g_geotiff.tif")
crs(elev) <- crs(countries10)
elev <- projectRaster(elev, crs = newcrs)


countries10 <- spTransform(countries10, newcrs)
coastline10 <- spTransform(coastline10, newcrs)
ports <- spTransform(ports, newcrs)


tic()

#study_area <- countries10[countries10$CONTINENT=="Asia",]
study_area <- countries10[countries10$ADMIN=="Iceland",]
#study_area <- countries10

ports_study_area <- ports
buffer <- gBuffer(study_area, width = 15)
coastline_study_area <- gIntersection(buffer, coastline10) 

#Note: dx denotes the spacing of two horizontaly adjacent points

hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)


# clue to reproject the maps at a later point
hexagons[1]
elev_cropped = crop(elev, buffer)
elev_masked = mask(elev_cropped, buffer)


library(tmap)
library(viridis)

tm_shape(hill) + tm_raster(n=100)













## Generates the coast line dataset 
make_raster <- function(x){
  intersection <- gIntersection(study_area, x)
  overlap <- bind(x, intersection)
  rr <- raster(extent(overlap), res=1.5)
  rr <- rasterize(overlap, rr)
  rm <- raster::as.matrix(rr)
  rm[is.na(rm)] = 0
  as.vector(t(rm))
}

coast_log <- rep(NA, length(hex_points@coords))
for (i in 1:nrow(hex_points@coords)){
  coast_log[i] <- gIntersects(coastline_study_area, hexagons[i])==TRUE
  print(c(i/nrow(hex_points@coords), i))
}
coast_hexagons <- hexagons[coast_log]
# coast_hexagons <- hexagons[sapply(1:nrow(hex_points@coords), function(x) gIntersects(coastline_study_area, hexagons[x]))==TRUE]

coast_data <- matrix(nrow=length(coast_hexagons@polygons), ncol = 460)
for (i in 1:length(coast_hexagons@polygons)){
  coast_data[i,] <- make_raster(coast_hexagons[i])
  print(c(i/length(coast_hexagons@polygons), i))
}

#coast_data <- t(sapply(1:length(coast_hexagons@polygons), function(x) make_raster(coast_hexagons[x])))

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
data <- data.frame(df) %>% dplyr::select(V462, everything()) %>% rename(y = V462) 
row.names(data) <- data$ID
data <- subset(data, select = -ID)

dataset_iceland <<- data
hexagons_iceland <<- hexagons
study_area_iceland <<- study_area
ports_study_area_iceland <<- ports_study_area

toc()

#save.image(file = "output/my_work_space.RData")
























# Test area
study_area <- countries10[countries10$ADMIN=="Iceland",]
buffer <- gBuffer(study_area, width = 30)
elev_cropped = crop(elev, buffer)
elev_masked = mask(elev_cropped, buffer)

# change values of a raster
elev_masked[elev_masked < -1] <- 0
tm_shape(elev_masked) + tm_raster(n=100) + tm_shape(study_area) + tm_fill(alpha=0.5) +
  tm_shape(coastline_study_area) + tm_lines()



tm_shape(elev_masked) + tm_raster(n=100) + tm_shape(study_area) + tm_fill(alpha=0.5) +
  tm_shape(coastline_study_area) + tm_lines() +tm_shape(hexagons) + tm_borders(alpha=0.5)



srtm_cropped = crop(elev_masked, coast_hexagons[35])
srtm_masked = mask(srtm_cropped, coast_hexagons[35])
tm_shape(srtm_masked) + tm_raster(n=100)
#study_area <- spTransform(study_area, newcrs)








# Look more at the performance of the model









tm_shape(sps_df) +
  tm_fill(col="prediction", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons_full) + tm_borders(col = "white") +
  tm_shape(ports_iceland) + tm_dots(size=0.1) 


hexagons <- gIntersection(hexagons, study_area, byid = TRUE)
row.names(dataset) <- paste(row.names(dataset), " 188", sep="")
cut_data <- dataset[row.names(dataset)  %in%  sapply(hexagons@polygons, function(x) x@ID),]
sps_df <- SpatialPolygonsDataFrame(hexagons, cut_data, match.ID = TRUE)

tm_shape(sps_df) +
  tm_fill(col="y") + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") 

+
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1)  




## To do:
## combine the predicted port data with the aggregated raster




















## ROC in ggplot
x_rf <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf <- y_rf[x_rf<=1]
x_rf <- x_rf[x_rf<=1]

model <- ranger(formula= y~., data=training_data, num.trees = 2, mtry = 10)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

x_rf1 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf1 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf1 <- y_rf1[x_rf1<=1]
x_rf1 <- x_rf1[x_rf1<=1]



model <- ranger(formula= y~., data=training_data, num.trees = 30, mtry = 150)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

x_rf2 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf2 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf2 <- y_rf2[x_rf2<=1]
x_rf2 <- x_rf2[x_rf2<=1]


model <- ranger(formula= y~., data=training_data, num.trees = 50, mtry = 80)

prediction <- predict(model, testing_data)$predictions
pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")
x_rf3 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf3 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf3 <- y_rf3[x_rf3<=1]
x_rf3 <- x_rf3[x_rf3<=1]



ggplot() + 
  geom_line(aes(x_rf, y_rf), color="Blue") + 
  geom_line(aes(x_rf1, y_rf1), color="Red") +
  geom_line(aes(x_rf2, y_rf2), color="Red") +
  geom_line(aes(x_rf3, y_rf3), color="Orange") +
  xlab("False positive rate") + ylab("True positive rate") + ggtitle("Receiver operating characteristic") +
  geom_segment(aes(x = 0, y = 0, xend = 0.98, yend = 1, colour = "segment"),alpha=0.3, linetype = 2) +
  scale_x_continuous(limits = c(0, 1)) + theme_classic() 






# Fitting the model 
model_testing <- ranger(formula= y~., data=dataset, num.trees = 300, mtry = 40)
prediction <- predict(model_testing, dataset)$predictions

dataset$prediction <-  prediction

# Combining the data
sps_df <- c()
sps_df <- SpatialPolygonsDataFrame(hexagons_africa, dataset_africa, match.ID = TRUE)
sps_df <- sps_df[sps_df$prediction==1,]

# Counting over the countries
n_harbors <- c()
# use sapply here:
for (i in study_area@data$ADMIN){
  nr <- sum(sapply(1:nrow(sps_df), function(x) gIntersects(sps_df[x,], study_area[study_area$ADMIN==i,])))
  n_harbors[which(study_area_africa@data$ADMIN==i)] <- nr
  print(c(i, nr))
}





study_area_country <- countries10[countries10$ADMIN=="Iceland",]
dataset_country <- gIntersection(sps_df, study_area_country, byid = TRUE)
row.names(dataset_country) <- gsub("\\s.*", "", sapply(dataset_country@polygons, function(x) x@ID))
sps_df_temp <- sps_df[row.names(sps_df) %in% sapply(dataset_country@polygons, function(x) x@ID), ]

sum(ifelse(sps_df_temp@data$y_pred>0.5,1,0))



# shows the roc of a pruned random forest

elev_tmp <- raster("ETOPO1_Ice_g_geotiff.tif") 
countries10_tmp <- ne_download(scale = 10, type = 'countries', category = 'cultural')
spain <- countries10_tmp[countries10$ADMIN=="Sweden",]

# Projecting the shapefile
crs(elev_tmp) <- crs(countries10_tmp)

# Note: in general buffering should be done on projected data. However,
#       here it is fine since it just captures the whole area.

# Buffer around region
spain_buffer <- gBuffer(spain, width = 1)
elev_cropped = crop(elev_tmp, spain_buffer)
elev_masked = raster::mask(elev_cropped, spain_buffer)
elev_masked[elev_masked < 0] <- 0

plot(elev_masked)
plot(spain,add=TRUE)



# Set the same projection (units are in km)
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
spain <- spTransform(spain, newcrs)
spain_buffer <- spTransform(spain_buffer, newcrs)
elev <- projectRaster(elev_masked, crs = newcrs)


# Generating the hexagons
size <- 30
hex_points_tmp <- spsample(spain_buffer, type = "hexagonal", cellsize = size)
hex_grid_tmp <- HexPoints2SpatialPolygons(hex_points_tmp, dx = size)
hexagons_tmp <- gIntersection(hex_grid_tmp, spain, byid = TRUE)
elev_simple <- aggregate(elev, fact=4)

# The raster can be simplified to speed up the calculations
elevations <- sapply(1:length(hexagons_tmp@polygons), function(x) mean(values(mask(elev_simple, hexagons_tmp[x])), na.rm=TRUE))
ID <- sapply(hexagons_tmp@polygons, function(x) x@ID)
data_tmp <- data.frame(log(elevations))
row.names(data_tmp) <- ID
sps_df_tmp<- SpatialPolygonsDataFrame(hexagons_tmp, data_tmp, match.ID = TRUE)


library(tmap)

# Aggregating over the hexagons
tm_shape(sps_df_tmp) +
  tm_fill(col="log.elevations.", palette=plasma(256),n=20, labels = NULL)+
  tm_shape(hexagons_tmp) +
  tm_borders(col = "white",lwd = 0.067) + tm_layout(legend.show=FALSE)




# Basic relationship
countries_list@data %>% filter(n_harbors>0) %>% 
  group_by(n_harbors) %>%
  summarize(gdp=mean(GDP_MD_EST), obs=n()) %>% 
  ggplot(aes(x=log(n_harbors), y=log(as.numeric(gdp)), size=obs)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm", se = TRUE, color="Black", size=0.5) + 
  xlab("log(Harbors)") + labs(caption = "") + ylab("log(GDP per capita)") +
  theme(legend.position="none")

# By continent
countries10@data %>% filter(n_harbors>0) %>% 
  group_by(n_harbors) %>%
  mutate(gdp=(GDP_MD_EST/as.numeric(POP_EST))) %>%
  #summarize(gdp=mean(GDP_MD_EST/as.numeric(POP_EST)), obs=n()) %>% 
  ggplot(aes(x=log(n_harbors), y=log(as.numeric(gdp)),color=CONTINENT)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="Black") + 
  xlab("log(Harbors)") + labs(color = "Continents") + ylab("log(GDP per capita)") 

# With labels
countries10@data %>% filter(n_harbors>0) %>% 
  group_by(n_harbors) %>%
  mutate(gdp=(GDP_MD_EST/as.numeric(POP_EST))) %>%
  filter(CONTINENT!="Antarctica", CONTINENT!="Seven seas (open ocean)") %>% 
  #summarize(gdp=mean(GDP_MD_EST/as.numeric(POP_EST)), obs=n()) %>% 
  ggplot(aes(x=log(n_harbors), y=log(as.numeric(gdp)))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="Black") + 
  xlab("log(Harbors)") + labs(caption = "") + ylab("log(GDP per capita)") +
  geom_text(aes(label=SOVEREIGNT),hjust=0, vjust=0)  +
  theme(legend.position="none")

# First stage
countries_list@data %>% filter(n_harbors>0) %>% 
  group_by(harbors) %>%
  summarize(n_harbors=mean(n_harbors), obs=n()) %>% 
  ggplot(aes(x=log(harbors), y=log(n_harbors))) + geom_point(alpha=0.2, size=4, color="Blue") +
  geom_smooth(method = "lm", se = FALSE, color="Black", size=0.5) + 
  xlab("Harbors") + labs(caption = "") + ylab("Predicted harbors") +
  theme(legend.position="none") 

# To do: first stage, check the countries, set up the slides









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
study_area <- countries10[countries10$SOVEREIGNT=="Iceland",]
#study_area <- countries10

ports_study_area <- ports
buffer <- gBuffer(study_area, width = 0.2)
coastline_study_area <- gIntersection(buffer, coastline10) 

#Note: dx denotes the spacing of two horizontaly adjacent points

hex_points <- spsample(buffer, type = "hexagonal", cellsize = 0.4)
hexagons <- sapply(1:nrow(hex_points@coords), 
                   function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 0.5)) 
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











plot(elev, 
     axes=FALSE,
     alpha=0.5,   # sets how transparent the object will be (0=transparent, 1=not transparent)
     add=T)  # add=TRUE (or T), add plot to the previous plotting frame


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

#crs(elev) <- crs(countries10)

elev <- projectRaster(elev, crs = newcrs)
countries10 <- spTransform(countries10, newcrs)
coastline10 <- spTransform(coastline10, newcrs)
ports <- spTransform(ports, newcrs)



study_area <- countries10[countries10$SOVEREIGNT=="Iceland",]

buffer <- gBuffer(study_area, width = 30)

hexagon_cropped = crop(elev, study_area)
hexagon_masked = (mask(hexagon_cropped, study_area))




elmat = matrix(raster::extract(hexagon_masked,raster::extent(hexagon_masked),buffer=10),
               nrow=ncol(hexagon_masked),ncol=nrow(hexagon_masked))

elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map(na.rm=FALSE)


elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()


elmat %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(elmat), color="imhof2") %>%
  plot_map()

raymat = ray_shade(elmat,lambert = TRUE)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(raymat,0.7) %>%
  plot_map()


ambmat = ambient_shade(elmat)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(raymat,0.7) %>%
  add_shadow(ambmat,0.7) %>%
  plot_map()


elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.7) %>%
  add_shadow(ambmat,0.7) %>%
  plot_3d(elmat, na.rm=FALSE)



library(rayshader)
montereybay %>%
  sphere_shade() %>%
  add_shadow(ray_shade(elevation_matrix)) %>%
  add_shadow(ambient_shade(elevation_matrix)) %>%
  plot_3d()




montshadow = ray_shade(montereybay,zscale=50)
montamb = ambient_shade(montereybay,zscale=50)
montereybay %>% 
  sphere_shade(zscale=10,texture = "imhof1") %>% 
  add_shadow(montshadow,0.5) %>%
  add_shadow(montamb) %>%
  
  print(plot_3d(montereybay,zscale=50,fov=0,theta=-45,phi=45,windowsize=c(1000,800),zoom=0.75,
                water=TRUE, waterdepth = 0, wateralpha = 0.5,watercolor = "lightblue",
                waterlinecolor = "white",waterlinealpha = 0.3))





library(elevatr)

data(lake)
elevation <- get_elev_raster(lake, z = 13)

elmat2 = matrix(raster::extract(elevation,raster::extent(elevation),buffer=10000),nrow=ncol(elevation),ncol=nrow(elevation))
elmat3 <- elmat2[300:500,300:500]

elmat2 %>%
  sphere_shade() %>% 
  add_water(detect_water(elmat2)) %>%
  plot_map()


ambmat = ambient_shade(elmat2)

elmat2 %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat2), color="desert") %>%
  add_shadow(ray_shade(elmat2,zscale=3,maxsearch = 300),0.7) %>%
  add_shadow(ambmat,0.7) %>%
  plot_3d(elmat2,zscale=10,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800))






TAREMPAH <- ports[ports$PORT_NAME=="SANTIAGO DE CUBA",]
buffer1 <- gBuffer(TAREMPAH, width = 0.02)
elevation <- get_elev_raster(buffer1, z = 14)

hexagon_cropped = crop(elevation, buffer1)
hexagon_masked = (mask(hexagon_cropped, buffer1))
plot(hexagon_masked)


elmat2 = matrix(raster::extract(hexagon_masked,raster::extent(hexagon_masked),buffer=10000),nrow=ncol(hexagon_masked),ncol=nrow(hexagon_masked))
elmat2 %>%
  sphere_shade() %>% 
  #add_water(detect_water(elmat2), color="desert") %>%
  plot_map()


raymat = ray_shade(elmat2,lambert = TRUE)

elmat2 %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(raymat,0.7) %>%
  plot_map()












## To do:
## combine the predicted port data with the aggregated raster
# To do: use elevation raster data, without rasterizing etc., probably much faster
#        automate the port data in analysis.

# tallene gir null mening..., sjekk hva raster values egentlig inneholder








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

#study_area <- countries10[countries10$SOVEREIGNT=="Angola",]
#study_area <- countries10[countries10$TYPE=="Sovereign country",]
study_area <- countries10

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
  ext <- extract(elev, coast_hexagons[x],fun=NULL)
}


coast_log <- rep(NA, nrow(hex_points@coords))
for (i in 1:nrow(hex_points@coords)){
  coast_log[i] <- gIntersects(coastline_study_area, hexagons[i])==TRUE
  print(c(i/nrow(hex_points@coords), i))
}
coast_hexagons <- hexagons[coast_log]

coast_data <- matrix(0, length(coast_hexagons@polygons), 220)
for (i in 1:length(coast_hexagons@polygons)){
  coast_data[i,] <- make_raster(coast_hexagons[i])[[1]][1:220]
  print(c(i/length(coast_hexagons@polygons), i))
}

y <- as.matrix(sapply(1:length(coast_hexagons@polygons), 
                      function(x) ifelse((gIntersects(ports_study_area, 
                                                      coast_hexagons[x])), 1, 0)))
ID <- sapply(coast_hexagons@polygons, function(x) x@ID)
coast_data_final <- cbind(ID, y, coast_data)

## Generates the inland dataset 
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

#save.image(file = "output/my_work_space_9.RData")








library(rayshader)

#Here, I load a map with the raster package.
loadzip = tempfile() 
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))

#We use another one of rayshader's built-in textures:
elmat_shade<- elmat %>%
  sphere_shade(texture = "desert")

ambmat = ambient_shade(elmat)

elmat_shade %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat_shade), color="desert") %>%
  add_shadow(ray_shade(elmat_shade,zscale=3,maxsearch = 300),0.5) %>%
  add_shadow(ambmat,0.5) %>%
  print(plot_3d(elmat,zscale=10,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800)))





#Plotting a spherical texture map of the built-in`montereybay`dataset.
montereybay %>%sphere_shade(texture="desert") %>%plot_3d(montereybay,zscale=50)


montereybay %>%sphere_shade(texture="imhof2") %>%
  plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof2",waterlinecolor="white", waterlinealpha=0.5)
  


montshadow = ray_shade(montereybay,zscale=50,lambert=FALSE)
montamb = ambient_shade(montereybay,zscale=50)
montereybay %>% 
  sphere_shade(zscale=10,texture = "imhof1") %>% 
  add_shadow(montshadow,0.5) %>%
  add_shadow(montamb) %>%
  print(plot_3d(montshadow,montereybay,zscale=50,fov=0,theta=-45,phi=45,windowsize=c(1000,800),zoom=0.75,
          water=TRUE, waterdepth = 0, wateralpha = 0.5,watercolor = "lightblue",
          waterlinecolor = "white",waterlinealpha = 0.5))

  
  
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.5) %>%
  add_shadow(ambmat,0.5)
  
  
  
  
  
montereybay %>%
  sphere_shade(texture = "bw") %>%
  add_shadow(ray_shade(montereybay)) %>%
  add_shadow(ambient_shade(montereybay)) %>%
  add_water(detect_water(montereybay),color = "unicorn") %>%
  plot_3d(montereybay)

  




# figure captions:

library(FSAdata)
data(RuffeSLRH92)
library(FSA)
library(knitr)
library(captioner)

figs <- captioner(prefix="Figure")
tbls <- captioner(prefix="Table")

figs(name="LenFreq1","Length frequency of Ruffe captured in 1992.")
figs(name="WtFreq1","Weight frequency of Ruffe captured in 1992.")

hist(~length,data=RuffeSLRH92)

fig_nums <- captioner()
fig_nums("my_first_figure", "My first figure's caption.")

plot(cars)





library(plm)

summary(lm(log(trade) ~ log(harbors+1) + length + long + lat, data = combined))
summary(plm(log(trade) ~ log(n_harbors+1) + length + long + lat, data = combined, index = c("continent"), model = "within"))
summary(plm(log(rgdpe) ~ log(n_harbors+1) + length + long + lat, data = combined, index = c("continent"), model = "within"))
summary(lm(log(rgdpe) ~ (n_harbors) + c_area, data = combined))
summary(plm(polity2 ~ (n_harbors) + c_area, data = combined, index = c("continent"), model = "within"))


ggplot(data=combined, aes(y=log(1+harbors), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm",col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(trade), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(rgdpe), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(pop), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)



ggplot(data=combined, aes(y=log(trade), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(rgdpe), x = log(1+n_harbors)))+ geom_point()
ggplot(data=combined, aes(y=(polity2), x = log(1+n_harbors)))+ geom_point()+
  geom_text(aes(label=country_code),hjust=0, vjust=0)







library(AER)

rf_gdp1 <- lm(pop ~ n_harbors , data=combined)
rf_gdp2<- lm(pop ~ n_harbors + area, data=combined)

iv_gdp1 <- ivreg(pop ~ harbors | n_harbors , data=dta_tmp)
iv_gdp2<- ivreg(pop ~ harbors + area| n_harbors + area, data=dta_tmp)

stargazer(rf_gdp1, rf_gdp2, type="latex", 
          star.char = c(""), dep.var.caption = "",
          dep.var.labels.include = FALSE, column.labels   = c("OLS", "IV"),
          column.separate = c(2, 2),notes = "The dependent variable is the logarithm of population.", notes.append = FALSE,
          model.names = FALSE, covariate.labels = c("Harbors", "Area", "Ports"), header=FALSE,font.size="tiny", omit.stat = c("rsq", "f"))
#,single.row = TRUE,column.sep.width = "1pt"








## Averaging over raster files, fungerer
pop_density <- raster("data/population_density/gpw_v4_population_density_rev10_2010_2pt5_min.tif")

#study_area <- countries10[countries10$CONTINENT=="Sweden",]

## Averaging by country
#hexagons_samerica










#joining country polygons with the predictions, 

# Iceland
training_data <- training_data[sample(1:nrow(data)),]
model_testing <- ranger(formula= y~., data=data, num.trees = 300, mtry = 40)
prediction <- predict(model_testing, dataset_africa)$predictions
prediction <- ifelse(prediction>=0.6, 1, 0)

table(dataset_iceland$y, prediction)
dataset_africa$prediction <-  prediction


# den tror alt langs vannet er en port hvis man ikke trener den med innlandet.


#hexagons_full <- gIntersection(hexagons_iceland, study_area, byid = TRUE)

#ID <- sapply(hexagons_iceland@polygons, function(x) x@ID)
#eval_df <- data.frame(dataset_iceland$y, prediction)
#row.names(eval_df) <- ID
sps_df <- c()
sps_df <- SpatialPolygonsDataFrame(hexagons_africa, dataset_africa, match.ID = TRUE)
sps_df <- sps_df[sps_df$prediction==1,]

tm_shape(study_area_namerica) +
  tm_borders() +
  tm_shape(sps_df) +
  tm_fill(col="prediction", palette = plasma(256)) + tm_layout(frame=FALSE) 



n_harbors <- c()
for (i in study_area_africa@data$ADMIN){
  nr <- sum(sapply(1:nrow(sps_df), function(x) gIntersects(sps_df[x,], study_area_africa[study_area_africa$ADMIN==i,])))
  n_harbors[which(study_area_africa@data$ADMIN==i)] <- nr
  print(c(i, nr))
}


study_area_africa@data$n_harbors <- n_harbors


gdp_cap <- study_area_africa@data$GDP_MD_EST / as.numeric(study_area_africa@data$POP_EST)
study_area_africa@data$gdp_cap <- gdp_cap
study_area_africa@data$n_harbors <- study_area_africa@data$n_harbors / as.numeric(study_area_africa@data$POP_EST)


plot(log(study_area_africa@data$n_harbors), log(study_area_africa@data$gdp_cap))

ggplot(data=study_area_africa@data, aes(x=log(n_harbors), y=log(gdp_cap))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


final <- rbind(study_area_namerica, study_area_samerica, study_area_africa, study_area_europe)


ggplot(data=final@data, aes(x=log(n_harbors), y=log(gdp_cap))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)










ids <- sapply(hexagons@polygons, function(x) x@ID)


first <- ids[ids=="ID1"]

which(ids=="ID113975")



hexagons@polygons[13976]


spain <- hexagons[13976]

# Buffer around region
spain_buffer <- gBuffer(coast_hexagons[200], width = 1000)
elev_cropped = crop(elev_projected, spain_buffer)

elev_cropped[elev_cropped<0] <- 0
mean(values(crop(elev_projected, spain)))

plot(elev_cropped)
plot(spain,add=TRUE)
plot(countries10,add=TRUE)













lights_reg_data <-  sps_df_coastal@data

lights_reg_data1 <- lights_reg_data %>% 
  mutate(density=ifelse(!is.nan(density_data),density_data,0))
lights_reg_data1 <- lights_reg_data %>% filter(!is.nan(density_data))

lights_reg_data1 <- lights_reg_data[!is.nan(lights_reg_data$density_data),]
lights_reg_data1 <- lights_reg_data[!is.na(lights_reg_data$density_data),]




















# Share of gpd
commodity_data <- read_excel("data/API_TX.VAL.MMTL.ZS.UN_DS2_en_excel_v2_10404372.xls", skip=3) %>% 
  dplyr::select(-"Indicator Name", -"Indicator Code", -"Country Code") %>% 
  gather("year", "share", 2:60) %>% 
  filter(year>=1997, !is.na(share)) 

## Commodity prices
commodity_data <- read_excel("data/CMOHistoricalDataAnnual.xlsx", sheet="Annual Prices (Real)",skip=8) %>% 
  dplyr::select(X__1,KSILVER,KPLATINUM,KGOLD,KZinc,KNICKEL,KTin,KLEAD,
                KCOPPER,KIRON_ORE,KALUMINUM,KPOTASH) %>% 
  rename(year=X__1, Silver=KSILVER,Platinum=KPLATINUM,Gold=KGOLD,
         Zinc=KZinc,Nickel=KNICKEL,Tin=KTin,Lead=KLEAD,Copper=KCOPPER,
         Iron=KIRON_ORE,Aluminum=KALUMINUM,Potash=KPOTASH) %>% 
  filter(year>=1990, year<=2015) %>% 
  gather("year metal", "price", 2:12) 

colnames(commodity_data)[2] <- "metal"
commodity_data$metal <- as.factor(commodity_data$metal)

commodity_data <- commodity_data %>% 
  group_by(metal) %>% 
  mutate(price_1990=ifelse(year==1990,price,0)) %>% 
  mutate(price_1990=max(price_1990)) %>% 
  ungroup() %>% 
  mutate(price=price/price_1990)

ggplot(data=commodity_data, aes(x=year, y=price,color=metal))+geom_line(alpha=0.9) +
  xlab("") + ylab("") +
  ggtitle("Prices (normalized)") +
  geom_vline(xintercept=2003, linetype = "longdash",alpha=0.6)+
  theme(legend.title = element_blank()) 

lead og potash mangler
## Commodity production

metals <- c("aluminum", "copper", "gold", "iron_ore", "nickel", "platinum",
            "silver", "tin", "zinc")

for (m in metals){
  path <- paste(paste("data/commodities/production/",m,sep = "")
                ,"/statisticsExport (1).xlsx" ,sep = "")
  #metal <- read_excel(path, skip=1)
  print(path)
}


metal <- read_excel("data/commodities/production/iron_ore/statisticsExport (2).xlsx", skip=1) %>% 
  rename("2001"=X__1,"2002"=X__2,"2003"=X__3,"2004"=X__4,"2005"=X__5,"2006"=X__6,
         "2007"=X__7,"2008"=X__8,"2009"=X__9,"2010"=X__10, "country"="\n\tCountry") %>% 
  dplyr::select(country, "2001","2002","2003","2004","2005","2006","2007","2008",
                "2009","2010") %>% 
  slice(1:55) %>% 
  gather("country year", "tonnes", 2:11) %>% 
  rename("year"="country year") %>% 
  filter(!is.na(tonnes)) %>% 
  mutate(country=as.factor(country),year=as.numeric(year))

metal_normalized <- metal %>% 
  group_by(country) %>% 
  mutate(tonnes_1990=ifelse(year==2001,tonnes,0)) %>% 
  mutate(tonnes_1990=max(tonnes_1990)) %>% 
  ungroup() %>% 
  mutate(tonnes=tonnes/tonnes_1990)

ggplot(data=metal_normalized, aes(x=year, y=tonnes,color=country))+geom_line() +
  xlab("") + ylab("") +
  ggtitle("Prices (normalized)") +
  geom_vline(xintercept=2003, linetype = "longdash",alpha=0.6)+
  theme(legend.title = element_blank()) 






```{r, fig.align="center", echo=FALSE, message=FALSE, warning=FALSE,results='asis', fig_caption= "Summary statistics"}

if (!require(pacman)) install.packages("pacman")
p_load(readxl, naniar, countrycode)

# Coordinated in the current CRS
long <- rep(0, nrow(countries_list@data))
lat <- rep(0, nrow(countries_list@data))

for (i in countries_list@data$SOVEREIGNT){
  long[which(countries_list@data$SOVEREIGNT==i)] <- extent(countries_list[countries_list$SOVEREIGNT==i,])[1]
  lat[which(countries_list@data$SOVEREIGNT==i)] <- extent(countries_list[countries_list$SOVEREIGNT==i,])[3]
}

countries_list@data$long <- long
countries_list@data$lat <- lat

#######################################
## Generating the country level data ##
#######################################

if (!require(pacman)) install.packages("pacman")
p_load(readxl, naniar, countrycode)


# Trade 
trade_data <- read_excel("/Users/sebastianellingsen/Dropbox/ports_ml/data/Trade_of_Goods.xlsx", skip=5) %>% 
  dplyr::select(-"Base Year", -"Scale") %>% 
  replace_with_na_all(condition=~.x=="...") %>% 
  slice(1:185) %>% 
  gather("year", "trade",2:167) %>% 
  filter(year==2010, !is.na(trade)) %>% 
  mutate(trade=as.numeric(trade), year=as.numeric(year))

country_code <- sapply(trade_data$Country, 
                       function(x) countrycode(x, 'country.name', 'iso3c'))
trade_data$country_code <- country_code 
trade_data <- trade_data[!is.na(trade_data$country_code),]

# PWT data
econ_data <- read_excel("/Users/sebastianellingsen/Dropbox/ports_ml/data/pwt90.xlsx", sheet="Data") %>% 
  filter(year==2010) %>% 
  rename(country_code=countrycode)

# Urban population
urban_data <- read_excel("/Users/sebastianellingsen/Dropbox/ports_ml/data/urban_population.xls", sheet="Data", skip=3) %>% 
  rename(urban="2010", country_code="Country Code") %>% 
  dplyr::select(country_code, urban)

# Data on ports and harbors
region <- c("MAC","HKG","GRL","ALA","CUW","SXM","ABW","JEY","GGY","IMN")
harbor_data <- countries_list@data %>% 
  mutate(country=SOVEREIGNT, country_code=ISO_A3, continent=CONTINENT) %>% 
  dplyr::select(n_harbors, harbors, country, c_area, country_code, continent, long, lat) %>% 
  filter(!(country_code %in% region), country!="Northern Cyprus", country!="Kosovo")

harbor_data[which(harbor_data$country=="Norway"),5] <- "NOR"
harbor_data[which(harbor_data$country=="France"),5] <- "FRA"

# Polity iv 
polity_data <- read_excel("/Users/sebastianellingsen/Dropbox/ports_ml/data/p4v2017.xls") %>% 
  dplyr::select(scode, country, year, polity2, democ) %>% filter(year==2010) 
polity_data$country_code <- sapply(polity_data$country, 
                                   function(x) countrycode(x, 'country.name', 'iso3c'))

# Coastline:
coastline_data <- read_excel("/Users/sebastianellingsen/Dropbox/ports_ml/data/coastline.xlsx", col_names=c("country", "length"))
coastline_data$country_code <- sapply(coastline_data$country, 
                                      function(x) countrycode(x, 'country.name', 'iso3c'))
coastline_data <- coastline_data %>% filter(!is.na(country_code))

# Combining datasets
combined <- inner_join(econ_data, harbor_data, by = "country_code") %>% 
  inner_join(polity_data, by = c("country_code")) %>% 
  inner_join(coastline_data, by = c("country_code")) %>%
  inner_join(urban_data, by = c("country_code")) %>% 
  filter(length>0) 


# Summary statistics
combined <- combined %>%  
  mutate(gdp=log(rgdpo/pop),n_harbors=log(1+n_harbors), harbors=log(1+harbors)) 

combined %>%  
  dplyr::select(harbors, n_harbors, gdp, pop, c_area, length, urban,urban) %>% 
  as.data.frame%>% stargazer(type="latex",style="io",                           covariate.labels=c("Ports","Harbors","GDP","Population","Exports","Area", "Polity", "Urban population", "Coastline length (km)"), header=FALSE, column.separate = c(2, 2),omit.summary.stat = c("p25","p75"))

```





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

study_area <- countries10[countries10$ADMIN=="Iceland",]
#study_area <- countries10[countries10$TYPE=="Sovereign country",]
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

coast_data <- matrix(nrow=length(coast_hexagons@polygons), ncol = 288)
for (i in 1:length(coast_hexagons@polygons)){
  coast_data[i,] <- make_raster(coast_hexagons[i])[1:288]
  print(c(i/length(coast_hexagons@polygons), i))
}

y <- as.matrix(sapply(1:length(coast_hexagons@polygons), 
                      function(x) ifelse((gIntersects(ports_study_area, 
                                                      coast_hexagons[x])), 1, 0)))
ID <- sapply(coast_hexagons@polygons, function(x) x@ID)
coast_data_final <- cbind(ID, y, coast_data)

## Generates the inland dataset 
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

#save.image(file = "output/my_work_space_15.RData")













ports_study_area <- ports
buffer <- gBuffer(study_area, width = 15)

coastline_study_area <- gIntersection(buffer, coastline10) 
buffer1 <- gBuffer(coastline_study_area, width = 15)

#Note: dx denotes the spacing of two horizontaly adjacent points
hex_points <- spsample(coastline_study_area, type = "stratified",
                       n=100,nsig="pretty")
hexagons <- sapply(1:nrow(hex_points@coords), 
                   function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)




r <- raster(study_area, resolution = 25)
r <- rasterize(study_area, r, field = 1)





