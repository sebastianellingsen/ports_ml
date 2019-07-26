## This file adds grid cell level data to the main dataset

## loading data

crs_south_america <- "+proj=moll +datum=WGS84 +units=km +pm=bogota"

# Weather and climate data
bio = getData('worldclim', var='bio', res=2.5, lon=22.440823, lat=5.539446)

for(i in 1:20) { 
  
  k <- projectRaster(aggregate(raster(bio, layer=i), 2), 
                     crs = crs_south_america, 
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
                          crs = crs_south_america, 
                          method = "bilinear"))
}
# Other raster files
# Sugarcane  
sugarcane <- raster("data/crops/sugarcane/res03_crav6190h_sihr_suc.tif")
sugarcane <- projectRaster(sugarcane, 
                           crs = crs_south_america, 
                           method = "bilinear")

# locations of mineral depposits
mines <- readOGR("data/mines/ofr20051294/ofr20051294.shp", 
                 "ofr20051294")
mines <- spTransform(mines, crs_south_america)

# Elevation data
elev <- raster("data/prepared_rasters/elev.grd")

# reproject elevation and find the slope and tr index, add luminosity etc. 

rasters  <- c(bio1,    bio2,   bio3,  bio4,  bio5,  bio6,   bio7,  
              bio8,    bio9,   bio10, bio11, bio12, bio13, bio14,  
              bio15,   bio16,  bio17, bio18, bio19, bio20, coffee, 
              tobacco, cotton, sugarcane)

# Function to extract the raster values for each grid cell
extracting_raster_info <- function(x){
  
  cell             <- south_america[south_america@data$ID==x,]
  cell_values      <- sapply(rasters, 
                             function(raster) raster::extract(raster, cell))
  return(sapply(cell_values, function(x) mean(x, na.rm=TRUE)))

}

# Adding the information from the raster to dataframe
values          <- lapply(south_america@data$ID, 
                         function(x) extracting_raster_info(x))
controls        <- as.data.frame(do.call(rbind, values))
names(controls) <- c("bio1",  "bio2",  "bio3",    "bio4",   "bio5",   "bio6", 
                     "bio7",  "bio8",  "bio9",    "bio10",  "bio11",  "bio12", 
                     "bio13", "bio14", "bio15",   "bio16",  "bio17",  "bio18", 
                     "bio19", "bio20", "coffee",  "tobaco", "cotton", "sugar")

# Adding the dataframe to the grid 
south_america@data <- cbind(south_america@data, controls)


## Imputing missing values 

# Finding the missing cells  
missing_rows <- south_america[apply(is.na(south_america@data), 1, any),]$ID

# This function takes the nearest cells of the missing cell and imputed the 
# control with a simple average 

neighbors_imputation <- function(x){
  
  # Accessing the cell with missing data 
  cell <- south_america[south_america@data$ID==x,]
  
  # Calculating the distance to other cells
  gd <-  gDistance(south_america,
                   cell,
                   byid=TRUE)
  
  # Finding the closest cells 
  distances1 <- cbind("ID"=colnames(gd), "distance"=gd[1:dim(gd)[2]]) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    mutate(distance=as.numeric(distance)) %>% 
    arrange(distance) %>% 
    top_n(-7) %>% 
    filter(ID!=rownames(gd))
  
  # Neighboring cells 
  neighbors <- south_america[south_america@data$ID %in% distances1$ID, ]
  
  # Imputing the missing values
  missing_rows_number <- which(south_america@data$ID==x)
  missing_variable    <- which(is.na(cell@data)) 
  data_for_imputation <- neighbors@data[,missing_variable] %>% as.data.frame()
  imputed_value       <- apply(data_for_imputation, 2, mean, na.rm=TRUE) 
  
  return(cbind(missing_rows_number, missing_variable, imputed_value))
}

# Extracting a list of imputed values
imputed_list <- lapply(missing_rows,
                       function(x) neighbors_imputation(x))

# Adding imputed values to the missing cells 
for(i in 1:114){
  missing_rows_number <- imputed_list[[i]][1]
  missing_variable <- imputed_list[[i]][,2]
  imputed_value <- imputed_list[[i]][,3]
  
  south_america@data[missing_rows_number, missing_variable] <- imputed_value
}


## Adding dummy variable for coastal areas







