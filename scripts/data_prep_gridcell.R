## This file adds grid cell level data to the main dataset

## loading data

# Weather and climate data
bio = getData('worldclim', var='bio', res=2.5, lon=22.440823, lat=5.539446)

for(i in 1:20) { 
  
  k <- projectRaster(aggregate(raster(bio, layer=i), 2), 
                     crs = newcrs, 
                     method = "bilinear")
  assign(paste("bio", i, sep = "") , k)
  print(i)
}

# Crop suitability 
crop <- c("coffee", "tobacco", "cotton") 
for(i in crop) { 
  
  i2 <- substr(i, 1, 3)
  path1 <- paste("data/crops", i, sep="/")
  path2 <- paste(paste("res03_crav6190l_silr", i2, sep="_"), ".tif", sep="")
  
  assign(i, projectRaster(raster(paste(path1, path2, sep="/")), 
                          crs = newcrs, 
                          method = "bilinear"))
}
# Other raster files
# Sugarcane  
sugarcane <- raster("data/crops/sugarcane/res03_crav6190h_sihr_suc.tif")
sugarcane <- projectRaster(sugarcane, 
                           crs = newcrs, 
                           method = "bilinear")

# locations of mineral depposits
mines <- readOGR("data/mines/ofr20051294/ofr20051294.shp", 
                 "ofr20051294")
mines <- spTransform(mines, newcrs)

# Elevation data
elev <- raster("data/prepared_rasters/elev.grd")



## Extract information from a buffer around each port
a                      <- gBuffer(pports, width = 5, byid = T)
rasters                <- c(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, 
                            bio9, bio10, bio11, bio12, bio13, bio14, bio15, 
                            bio16, bio17, bio18, bio19, bio20, coffee, tobacco, 
                            cotton, sugarcane)

extracting_raster_info <- function(x){
  
  b <- raster::extract(x, a)
  return(sapply(1:length(b), 
                function(x) min(b[[x]],
                                na.rm = TRUE)))
}
output <- lapply(rasters, function(x) extracting_raster_info(x))

names(output) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                   "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", 
                   "bio16", "bio17", "bio18", "bio19", "bio20", "coffee", "tobaco", 
                   "cotton", "sugar")

controls <- output %>% as.tibble() %>% unnest()



















