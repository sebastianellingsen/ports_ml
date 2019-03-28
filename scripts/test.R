library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(rnaturalearth)
library(viridis)
library(rgdal)
library(rgeos)
# Many geometry operations in sf, for example, assume their inputs have a projected CRS, 
# because the GEOS functions they are based on assume projected data.

elev <- raster("ETOPO1_Ice_g_geotiff.tif") 
countries10 <- ne_download(scale = 10, type = 'countries', category = 'cultural')
spain <- countries10[countries10$ADMIN=="Denmark",]

# Projecting the shapefile
crs(elev) <- crs(countries10)

# Note: in general buffering should be done on projected data. However,
#       here it is fine since it just captures the whole area.

# Buffer around region
spain_buffer <- gBuffer(spain, width = 1)
elev_cropped = crop(elev, spain_buffer)
elev_masked = mask(elev_cropped, spain_buffer)
elev_masked[elev_masked < 0] <- 0

plot(elev_masked, asp=0.7)
plot(spain,add=TRUE)

# Set the same projection (units are in km)
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
spain <- spTransform(spain, newcrs)
spain_buffer <- spTransform(spain_buffer, newcrs)
elev <- projectRaster(elev_masked, crs = newcrs)



# Plotting the area
image(elev, col=inferno(256))
plot(spain, add=TRUE)

# Generating the hexagons
size <- 30
hex_points <- spsample(spain_buffer, type = "hexagonal", cellsize = size)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
hexagons <- gIntersection(hex_grid, spain, byid = TRUE)
elev_simple <- aggregate(elev, fact=4)

# The raster can be simplified to speed up the calculations
elevations <- sapply(1:length(hexagons@polygons), function(x) mean(values(mask(elev_simple, hexagons[x])), na.rm=TRUE))
ID <- sapply(hexagons@polygons, function(x) x@ID)
data <- data.frame(log(elevations))
row.names(data) <- ID
sps_df <- SpatialPolygonsDataFrame(hexagons, data, match.ID = TRUE)


library(tmap)

# Aggregating over the hexagons
tm_shape(sps_df) +
  tm_fill(col="log.elevations.", palette=plasma(256),n=20, labels = NULL)+
  tm_shape(hexagons) +
  tm_borders(col = "white",lwd = 0.067) + tm_layout(legend.show=FALSE)







