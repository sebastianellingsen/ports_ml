
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


p1 <- hex_points[1]
hex_grid1 <- HexPoints2SpatialPolygons(p1, dx = 30)

x <- gIntersection(iceland, hex_grid1)


# sa ma den lagres som en raster.

tm_shape(hex_grid1) +
  tm_borders(col = "grey")+
  tm_shape(x) +
  tm_fill(col = "grey") + tm_layout(frame=FALSE)


tm_shape(iceland) +
  tm_fill()+
  tm_shape(hex_grid) +
  tm_borders(col = "white")+
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1) + tm_layout(frame=FALSE)



as.tibble(x)

# transform to raster, then matrix, then to a tibble of datasets like in the number analysis.
ext <- extent(x)
rr <- raster(ext, res=0.5)
rr <- rasterize(x, rr)

tm_shape(rr)+tm_raster()

rm <- raster::as.matrix(rr)
rm[is.na(rm)] = 0

