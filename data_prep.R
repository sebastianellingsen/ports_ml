
## Loading packages
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos)

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

#elev <- projectRaster(elev, crs=newcrs)

# Subsetting to north and south america and creating the grid

iceland <- countries10[countries10$ADMIN=="Iceland",]
ports_iceland <- ports[ports$COUNTRY=="IC",]



buffer <- gBuffer(iceland, width = 15)

coastline_iceland <- gIntersection(buffer, coastline10) 
elev_crop <- crop(elev, buffer)


# hex - with clipping
hex_grid_c <- make_grid(iceland, type = "hexagonal", cell_area = 1200, clip = FALSE)
plot(iceland, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid_c, border = "orange", add = TRUE)
box()
# square - without clipping
sq_grid <- make_grid(study_area_utm, type = "square", cell_area = 625, clip = FALSE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(sq_grid, border = "orange", add = TRUE)
box()
# square - with clipping
sq_grid_c <- make_grid(iceland, type = "square", cell_area = 625, clip = TRUE)
plot(iceland, col = "grey50", bg = "light blue", axes = FALSE)
plot(sq_grid_c, border = "orange", add = TRUE)
box()



tm_shape(iceland) +
  tm_fill()+
tm_shape(hex_grid_c) +
  tm_borders(col = "white")+
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1) + tm_layout(frame=FALSE)
  


#bruk en buffer

x <- crop(polydf2, polydf1)


buffer


hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30, offset = c(0, 0))
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 30)
plot(study_area, col = "grey50", bg = "light blue", axes = TRUE)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)



## Dataframe with one row for each grid
hex_points <- spsample(iceland, type = "hexagonal", cellsize = 30)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 30)


p1 <- hex_points[20]
hex_grid1 <- HexPoints2SpatialPolygons(p1, dx = 30)
x <- gIntersection(iceland, hex_grid1)




y <- c()
hex_points <- spsample(iceland, type = "hexagonal", cellsize = 30)
df <- data.frame(matrix(ncol = 1050, nrow = 0))

for (i in 1:nrow(hex_points@coords)){
  
  point <- hex_points[i]
  
  # Generate a hexagon around point i
  hexagon <- HexPoints2SpatialPolygons(point, dx = 30)

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

# Final dataframe
df[,dim(df)[2]+1] <- y
df <- df[, colSums(df != 0) > 0]






# sa ma den lagres som en raster.

tm_shape(hex_grid1) +
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

