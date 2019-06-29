## This file samples points and fits the model 

buffer <- gBuffer(study_area, width = 100)
coastline_study_area <- gIntersection(coastline10, buffer)

sample <- spsample(coastline_study_area, 
                   5000, 
                   type = "random", 
                   method="Line")
sample <- sample[sample(length(sample)),] 
sample <- sample[1:3027]

elev_below <- raster("elev_below.grd")
tri        <- raster("tri.grd")

slope <- terrain(elev, 
                 opt='slope', 
                 unit='radians', 
                 neighbors=8)

# Buffer around each point
a <- gBuffer(sample, width = bwidth, byid=TRUE)

# Elevation in buffer
b <- raster::extract(elev, a)
tri_sample <- raster::extract(elev, a)
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

y <- rep(0, length(slope_sample)[1])


## Adding continent and distance to closest river 
cont <- c()
river_dist <- c()
k <- 1
for (i in 1:length(a@polygons)){
  
  ## Adding subregion
  point <- a[i]
  cont[k] <- over(point,countries10)$SUBREGION
  
  ## Measuring distance to river
  river_dist[k] <- gDistance(point, rivers50)
  
  print(k/length(a@polygons))
  k <- k+1
  
}
cont <- ifelse(is.na(cont), "na", cont)


## Adding information on the coastline length
len <- c()
k <- 1
for (i in 1:length(a@polygons)){
  
  len[i] <- rgeos::gLength(crop(coastline10, a[i]))
  
  print(k/length(a@polygons))
  k <- k+1
}

# Joining the data
sample_df <- cbind(mean_slope, 
                   mean_tri, 
                   min_elev,
                   max_elev, 
                   mean_elev,
                   elevation_p,
                   tri_p,
                   slope_p,
                   cont,
                   len,
                   river_dist) %>% 
              data.frame() 

# Adding the attributes to the shapefile 
ID <- sapply(a@polygons, function(x) x@ID)
row.names(sample_df) <- ID

sps_df_tmp <- SpatialPolygonsDataFrame(a, 
                                       sample_df, 
                                       match.ID = TRUE)

## Restricting the sample with the right variables
# sample_df <- sps_df_tmp@data %>%
#   filter(!is.na(slope)) 

# Fit model on the prediction dataset
# load("output/pred_dataframe3.rds")

df1 <- pred_dataframe1 
model1 <- ranger(formula= as.numeric(y)~., 
                 data=df1, 
                 num.trees = 5000, 
                 mtry =5)

prediction1 <- predict(model1, data=sample_df)
prediction1 <- prediction1[["predictions"]]
sample_df$pr_port <- prediction1
sample_df$prediction <- ifelse(prediction1>1.55,1,0)

## Matching with the spatial data
ID <- rownames(sample_df)
predicted_ports <- sps_df_tmp[rownames(sps_df_tmp@data)%in%ID,]
predicted_ports <- SpatialPolygonsDataFrame(predicted_ports, 
                                            sample_df, 
                                            match.ID = TRUE)
all_cells <- predicted_ports
predicted_ports <- predicted_ports[predicted_ports@data$prediction==1,]

# load("/Users/sebastianellingsen/Dropbox/ports_ml/africa.Rda")

# Adding distance of the ports and prediceted port
distance_pport <- c()
pr_port <- c()
distance_port_locations <- c()
k <- 1

for (i in south_america@data$ID){
  cell <- south_america[south_america@data$ID==i,]
  
  distance_pport[k] <- gDistance(predicted_ports, cell)
  distance_port_locations[k] <- gDistance(ports, cell)
  
  ## Accessing port probability
  pr_port[k] <- ifelse(!is.na(over(cell,all_cells)$pr_port), 
                      over(cell,all_cells)$pr_port, 1)
  
  k <- k+1
  print(k/length(row.names(south_america@data)))
}

south_america@data$dis_pport <- distance_pport
south_america@data$dis_port <- distance_port_locations
south_america@data$pr_port <- pr_port-1


