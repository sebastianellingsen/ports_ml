
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


generating_data <- function(country, port){
  
  ## Note: The function affects the global environment by 
  ## storing dataframes and sp objects in the workspace.
  
  # Subsetting to region of interest
  iceland <- countries10[countries10$ADMIN==country,]
  ports_iceland <- ports[ports$COUNTRY==port,]

  # Creating a buffer around the region of interest
  buffer <- gBuffer(iceland, width = 15)

  coastline_iceland <- gIntersection(buffer, coastline10) 
  #buffer_coastline <- gBuffer(coastline_iceland, width = 30)

  #elev_crop <- crop(elev, buffer)

  ## Generating the dataset ##

  y <- c()
  hex_points <- spsample(buffer, type = "hexagonal", cellsize = 30)
  hexagons <- sapply(1:nrow(hex_points@coords), function(x) HexPoints2SpatialPolygons(hex_points[x],dx = 30)) 
  hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
   flatten() %>% 
   do.call(rbind, .)

  df <- data.frame(matrix(ncol = 1050, nrow = dim(hex_points@coords)[1]))

  for (i in 1:nrow(hex_points@coords)){
  
   hexagon <- hexagons[i] 
  
    if(gIntersects(coastline_iceland, hexagon)){
    
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
  
  dataset <<- data
  hexagons <<- hexagons
  iceland <<- iceland
  ports_iceland <<- ports_iceland
  
}



