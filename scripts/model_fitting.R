## This file samples points and fits the model 

countries_list <- c("Chile", 
                    "Bolivia", 
                    "Peru", 
                    "Argentina", 
                    "Uruguay",   
                    "Ecuador", 
                    "Colombia", 
                    "Paraguay", 
                    "Venezuela", 
                    "Panama",
                    "El Salvador", 
                    "Honduras", 
                    "Costa Rica", 
                    "Guatemala", 
                    "Mexico", 
                    "Nicaragua", 
                    "Cuba", 
                    "Dominican Republic")

study_area <- countries10[countries10@data$ADMIN %in% countries_list, ]
buffer <- gBuffer(study_area, width = 100)
coastline_study_area <- gIntersection(coastline10, buffer)

sample <- spsample(coastline_study_area, 
                   5000, 
                   type = "random", 
                   method="Line")
sample <- sample[sample(length(sample)),] 


# Buffer around each point
a <- gBuffer(sample, width = bwidth, byid=TRUE)

# Elevation in buffer
b            <- raster::extract(elev, a)
tri_sample   <- raster::extract(elev, a)
slope_sample <- raster::extract(slope, a)

# Elevation 
elevation_p <- raster::extract(elev, sample)

# Terrain ruggedness 
tri_p <- raster::extract(tri, sample)

# Slope 
slope_p <- raster::extract(slope, sample)

# Min.elevation
min_elev <- sapply(1:length(b), 
                   function(x) min(b[[x]], 
                                   na.rm = TRUE))
# Max. Min.elevation
max_elev <- sapply(1:length(b), 
                   function(x) max(b[[x]], 
                                   na.rm = TRUE))
# Mean elevation
mean_elev <- sapply(1:length(b), 
                    function(x) mean(b[[x]], 
                                     na.rm = TRUE))
# Mean terrain ruggedness
mean_tri <- sapply(1:length(tri_sample), 
                   function(x) mean(tri_sample[[x]], 
                                    na.rm = TRUE))
# Mean slope
mean_slope <- sapply(1:length(slope_sample), 
                     function(x) mean(slope_sample[[x]],
                                      na.rm = TRUE))

layer <- raster::extract(wind, a)
wind_p <- sapply(1:length(layer), 
                 function(x) mean(layer[[x]],
                                  na.rm = TRUE))
# Imputing 23 NAs with the mean value 
wind_p <- ifelse(is.na(wind_p), mean(wind_p, na.rm=T), wind_p)



## Adding continent and distance to closest river 
cont        <- c()
river_dist  <- c()
k           <- 1
for (i in 1:length(a@polygons)){
  
  ## Adding subregion
  point   <- a[i]
  cont[k] <- over(point,countries10)$SUBREGION
  
  ## Measuring distance to river
  river_dist[k] <- gDistance(point, rivers50)
  k <- k+1
  
}
cont <- ifelse(is.na(cont), "na", cont)


## Adding information on the coastline length
coastline_length <- c()
k   <- 1
for (i in 1:length(a@polygons)){
  
  coastline_length[i] <- rgeos::gLength(crop(coastline10, a[i]))
  k <- k+1
}

# Joining the data
sample_df <- cbind(min_elev,
                   max_elev, 
                   elevation_p,
                   tri_p,
                   cont,
                   slope_p,
                   coastline_length,
                   wind_p,
                   river_dist) %>% 
              data.frame() 

sample_df <- sample_df %>% 
  mutate(min_elev         = as.numeric(as.character(min_elev)),
         max_elev         = as.numeric(as.character(max_elev)), 
         coastline_length = as.numeric(as.character(coastline_length)),
         river_dist       = as.numeric(as.character(river_dist)), 
         wind_p           = as.numeric(as.character(wind_p)), 
         elevation_p      = as.numeric(as.character(elevation_p)),
         tri_p            = as.numeric(as.character(tri_p)),
         slope_p          = as.numeric(as.character(slope_p))) %>% 
  filter(!is.na(slope_p))

write.csv(sample_df, "output/sample_df.csv", row.names = FALSE)


## Adding the attributes to the shapefile 
ID                   <- sapply(a@polygons, function(x) x@ID)
row.names(sample_df) <- ID

sps_df_tmp <- SpatialPolygonsDataFrame(a, 
                                       sample_df, 
                                       match.ID = TRUE)
# writeOGR(sps_df_tmp,
#          ".",
#          "output/sps_df_tmp",
#          driver="ESRI Shapefile",
#          overwrite=TRUE)

## Read the files
sps_df_tmp     <- readOGR("output/sps_df_tmp.shp", "sps_df_tmp")
colnames(sps_df_tmp@data) <- c('min_elev',
                               'max_elev',
                               'elevation_p',
                               'tri_p',
                               'cont',
                               'slope_p',
                               'coastline_length',
                               'wind_p',
                               'river_dist')
sps_df_tmp@data <- sps_df_tmp@data[,-5]

pred_dataframe <- read_csv("output/pred_dataframe.csv")



## Fit model on the prediction dataset
sample_df <- sps_df_tmp@data
model <- ranger(formula= as.numeric(y)~.,
                data=pred_dataframe,
                num.trees = 1000,
                mtry = 5)
prediction_p <- predict(model, sample_df)$predictions
prediction   <- ifelse(prediction_p>0.85,1,0)
prediction   <- ifelse(prediction_p<0.065,1,0)

sample_df$prediction <- prediction 
sample_df <- sample_df[order(-prediction),] 
# sample_df1 <- sample_df[1:93,]
sample_df1 <- sample_df

## Matching with the spatial data 
ID              <- rownames(sample_df1)
predicted_ports <- sps_df_tmp[rownames(sps_df_tmp@data)%in%ID,]
predicted_ports <- SpatialPolygonsDataFrame(predicted_ports, 
                                            sample_df1, 
                                            match.ID = TRUE)
predicted_ports@data$prediction_p <- prediction_p
predicted_ports_l <- predicted_ports[predicted_ports@data$prediction==1,]

writeOGR(predicted_ports_l,
         ".",
         "output/predicted_ports_l",
         driver="ESRI Shapefile",
         overwrite=TRUE)

# tm_shape(coastline_study_area)+tm_lines(alpha=0.5)+
#   tm_shape(predicted_ports)+tm_dots(size=0.5)

