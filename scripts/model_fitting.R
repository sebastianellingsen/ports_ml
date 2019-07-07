## This file samples points and fits the model 

sa_countries <- c("Argentina",
                  "Uruguay",
                  "Paraguay",
                  "Chile",
                  "Bolivia",
                  "Peru",
                  "Colombia",
                  "Venezuela",
                  "Ecuador",
                  "Brazil",
                  "Panama",
                  "Costa Rica",
                  "Nicaragua",
                  "Honduras",
                  "Guatemala",
                  "Mexico")

study_area <- countries10[countries10@data$ADMIN %in% sa_countries, ]
buffer <- gBuffer(study_area, width = 100)
coastline_study_area <- gIntersection(coastline10, buffer)

sample <- spsample(coastline_study_area, 
                   5000, 
                   type = "random", 
                   method="Line")
sample <- sample[sample(length(sample)),] 

elev       <- raster("data/prepared_rasters/elev.grd")
tri        <- raster("data/prepared_rasters/tri.grd")
slope      <- terrain(elev, 
                      opt='slope', 
                      unit='radians', 
                      neighbors=8)

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

y <- rep(0, length(slope_sample)[1])


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
  
  print(k/length(a@polygons))
  k <- k+1
  
}
cont <- ifelse(is.na(cont), "na", cont)


## Adding information on the coastline length
len <- c()
k   <- 1
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

sample_df <- sample_df %>% 
  mutate(mean_slope=as.numeric(as.character(mean_slope)), 
         mean_tri=as.numeric(as.character(mean_tri)), 
         min_elev=as.numeric(as.character(min_elev)),
         max_elev=as.numeric(as.character(max_elev)), 
         max_elev=as.numeric(as.character(max_elev)), 
         len=as.numeric(as.character(len)), 
         river_dist=as.numeric(as.character(river_dist)), 
         mean_elev=as.numeric(as.character(mean_elev)),
         elevation_p=as.numeric(as.character(elevation_p)),
         tri_p=as.numeric(as.character(tri_p)),
         slope_p=as.numeric(as.character(slope_p)))


## Adding polynomials
d <- c()
for(i in 2:5) { 
  
  k <- sapply(1:11, function(x) sample_df[,x]^i)
  
  colnames(k) <- paste(names[1:11], i, sep = "")
  
  d <- cbind(d, k)
}
polynomials <- d %>% as.data.frame() %>%  
  dplyr::select(-c(cont2, cont3, cont4, cont5))


## Adding interaction terms
inter1           <- sapply(2:11, 
                           function(x) sample_df[,1]*sample_df[,x])
colnames(inter1) <- paste0("mean_slope", colnames(sample_df)[2:11])

inter2           <- sapply(3:11, 
                           function(x) sample_df[,2]*sample_df[,x])
colnames(inter2) <- paste0("mean_tri", colnames(sample_df)[3:11])

inter3           <- sapply(4:11, 
                           function(x) sample_df[,3]*sample_df[,x])
colnames(inter3) <- paste0("min_elev", colnames(sample_df)[4:11])

inter4           <- sapply(5:11, 
                           function(x) sample_df[,4]*sample_df[,x])
colnames(inter4) <- paste0("max_elev", colnames(sample_df)[5:11])

inter5           <- sapply(6:11, 
                           function(x) sample_df[,5]*sample_df[,x])
colnames(inter5) <- paste0("mean_elev", colnames(sample_df)[6:11])

inter6           <- sapply(7:11, 
                           function(x) sample_df[,6]*sample_df[,x])
colnames(inter6) <- paste0("elevation_p", colnames(sample_df)[7:11])

inter7           <- sapply(8:11, 
                           function(x) sample_df[,7]*sample_df[,x])
colnames(inter7) <- paste0("tri_p", colnames(sample_df)[8:11])

inter8           <- sapply(11:11, 
                           function(x) sample_df[,11]*sample_df[,x])
colnames(inter8) <- paste0("len", colnames(sample_df)[11:11])


interactions <- cbind(inter1, inter2, inter3, inter4, inter5, 
                        inter6, inter7, inter8) %>% 
                as.data.frame() %>% 
                dplyr::select(-c(mean_tricont, tri_pcont, elevation_pcont, 
                                 mean_elevcont, max_elevcont, min_elevcont, 
                                 mean_tricont, mean_slopecont))


## Adding dummy variables
dummies         <-  model.matrix(~sample_df$cont) %>% as.data.frame()
pred_dataframe3 <- cbind(sample_df, polynomials, interactions, dummies) %>% 
  mutate("pred_dataframe$contAustralia and New Zealand" = 0,
         "pred_dataframe$contCaribbean"                 = 0,                
         "pred_dataframe$contCentral Asia"              = 0,         
         "pred_dataframe$contEastern Africa"            = 0,       
         "pred_dataframe$contEastern Asia"              = 0,             
         "pred_dataframe$contEastern Europe"            = 0,           
         "pred_dataframe$contMelanesia"                 = 0,                
         "pred_dataframe$contMicronesia"                = 0,               
         "pred_dataframe$contMiddle Africa"             = 0,            
         "pred_dataframe$contna"                        = 0,                       
         "pred_dataframe$contNorthern Africa"           = 0,          
         "pred_dataframe$contNorthern Europe"           = 0,          
         "pred_dataframe$contPolynesia"                 = 0,                
         "pred_dataframe$contSeven seas (open ocean)"   = 0,
         "pred_dataframe$contSouth-Eastern Asia"        = 0,       
         "pred_dataframe$contSouthern Africa"           = 0,          
         "pred_dataframe$contSouthern Asia"             = 0,            
         "pred_dataframe$contSouthern Europe"           = 0,          
         "pred_dataframe$contWestern Africa"            = 0,           
         "pred_dataframe$contWestern Asia"              = 0)



sample_df <- pred_dataframe3


## Adding the attributes to the shapefile 
ID                   <- sapply(a@polygons, function(x) x@ID)
row.names(sample_df) <- ID

sps_df_tmp <- SpatialPolygonsDataFrame(a, 
                                       sample_df, 
                                       match.ID = TRUE)


## Fit model on the prediction dataset
sample_df <- sps_df_tmp@data

y <- pred_dataframe1[,12]
x <- data.matrix(pred_dataframe1[,-12])

fit <- glmnet(x, 
             y, 
             family = "binomial", 
             standardize = T)

cvfit = cv.glmnet(x, y, family = "binomial", type.measure = "class")
lambda.min <- cvfit$lambda.min
prediction <- predict(fit, 
                      data.matrix(sample_df), 
                      type = "response", 
                      s = lambda.min)
sample_df$prediction <- as.numeric(ifelse(prediction>0.43,1,0))


## Matching with the spatial data
ID              <- rownames(sample_df)
predicted_ports <- sps_df_tmp[rownames(sps_df_tmp@data)%in%ID,]
predicted_ports <- SpatialPolygonsDataFrame(predicted_ports, 
                                            sample_df, 
                                            match.ID = TRUE)
# all_cells <- predicted_ports
predicted_ports <- predicted_ports[predicted_ports@data$prediction==1, ]






# # load("/Users/sebastianellingsen/Dropbox/ports_ml/africa.Rda")
# 
# # Adding distance of the ports and prediceted port
# distance_pport <- c()
# pr_port <- c()
# distance_port_locations <- c()
# k <- 1
# 
# for (i in south_america@data$ID){
#   cell <- south_america[south_america@data$ID==i,]
#   
#   distance_pport[k] <- gDistance(predicted_ports, cell)
#   distance_port_locations[k] <- gDistance(ports, cell)
#   
#   ## Accessing port probability
#   pr_port[k] <- ifelse(!is.na(over(cell,all_cells)$pr_port), 
#                       over(cell,all_cells)$pr_port, 1)
#   
#   k <- k+1
#   print(k/length(row.names(south_america@data)))
# }
# 
# south_america@data$dis_pport <- distance_pport
# south_america@data$dis_port <- distance_port_locations
# south_america@data$pr_port <- pr_port-1


