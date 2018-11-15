
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



buffer <- gBuffer(iceland, width = 1)

coastline_iceland <- gIntersection(buffer, coastline10) 
elev_crop <- crop(elev, buffer)


# hex - with clipping
hex_grid_c <- make_grid(iceland, type = "hexagonal", cell_area = 1200, clip = TRUE)
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
  tm_dots(shape = 1, size=0.1)
  





