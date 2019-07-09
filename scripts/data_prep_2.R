## This file prepares data for predicting using measures of the terrain directly

################################################################################
######################### Preparing data #######################################
################################################################################

## Preparing packages 
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, 
       sf, 
       raster, 
       tmap, 
       sp, 
       rgdal, 
       rnaturalearth, 
       rgeos, 
       viridis, 
       tictoc, 
       spatialEco, 
       ranger, 
       caret,
       lfe)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
bwidth <- 3

## Ports 
ports <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports <- ports[!is.na(ports$HARBORTYPE),]
ports <- ports[ports$HARBORTYPE=="CN",]


## Coastline
coastline10 <- ne_download(scale = 10, 
                           type = 'coastline', 
                           category = 'physical')

## Countries
countries10 <- ne_download(scale = 10, 
                           type = 'countries', 
                           category = 'cultural')

countries110 <- ne_download(scale = 110, 
                            type = 'countries', 
                            category = 'cultural')

## Rivers
rivers50 <- ne_download(scale = 50, 
                        type = 'rivers_lake_centerlines', 
                        category = 'physical')

## Elevation and terrain ruggedness data
elev       <- raster("data/prepared_rasters/elev.grd")
tri        <- raster("data/prepared_rasters/tri.grd")

slope <- terrain(elev, 
                 opt='slope', 
                 unit='radians', 
                 neighbors=8)

## Projecting the data 
countries10  <- spTransform(countries10, newcrs)
countries110 <- spTransform(countries110, newcrs)
coastline10  <- spTransform(coastline10, newcrs)
rivers50     <- spTransform(rivers50, newcrs)
ports        <- spTransform(ports, newcrs)



################################################################################
######################### Generating dataset ###################################
################################################################################

## Removing small countries
small <- c("Vanatu", 
           "San Marino", 
           "Vatican", 
           "Fiji", 
           "Solomon Islands",
           "Federated States of Micronesia", 
           "Palau", 
           "Samoa", 
           "Nauru")

study_area <- countries10[!(countries10$SOVEREIGNT %in% small),]


## Extracting values

## Port locations

# Buffer around each point
a <- gBuffer(ports, width = bwidth, byid=TRUE)

# Elevation in buffer
b <- raster::extract(elev, a)
tri_port <- raster::extract(elev, a)
slope_port  <- raster::extract(slope, a)

# Elevation 
elevation_p <- raster::extract(elev, ports)

# Terrain ruggedness 
tri_p <- raster::extract(tri, ports)

# Slope 
slope_p <- raster::extract(slope, ports)

# Min. elevation
min_elev <- sapply(1:length(b), 
                        function(x) min(b[[x]],
                                        na.rm = TRUE))
# Max. elevation
max_elev <- sapply(1:length(b), 
                        function(x) max(b[[x]],
                                        na.rm = TRUE))

# Mean election
mean_elev <- sapply(1:length(b), 
                           function(x) mean(b[[x]],
                                            na.rm = TRUE))

# Mean terrain ruggedness
mean_tri <- sapply(1:length(tri_port), 
                         function(x) mean(tri_port[[x]],
                                          na.rm = TRUE))

# Mean slope
mean_slope<- sapply(1:length(slope_port), 
                        function(x) mean(slope_port[[x]],
                                         na.rm = TRUE))

## Slope at the port
y <- rep(1, length(slope_port)[1])

# Adding continent
cont <- over(a, countries10)$SUBREGION
cont <- ifelse(is.na(cont), "na", cont)

# Measuring distance to river
river_dist <- sapply(a@data$INDEX_NO, 
                     function(i) gDistance(a[a@data$INDEX_NO==i,], rivers50))


## Sampling points on the the coastline
sample_coastline <- spsample(coastline10, 
                             500000, 
                             type = "regular", 
                             method="Line")
sample_coastline <- sample_coastline[sample(length(sample_coastline)),] 


## Calculating the closest sampled point to each port
port_on_coastline <- c()
len               <- c()
k                 <- 1
for (i in ports@data$INDEX_NO){
  
  point <- ports[ports@data$INDEX_NO==i,]
  
  ## Finding the closest on point on the coastline
  gd                <-  gDistance(sample_coastline, 
                                  point, 
                                  byid=TRUE)

  port_on_coastline <-  sample_coastline[apply(gd, 1, which.min)]

  ## Measuring the coastal indentation
  port_on_coastline  <-  gBuffer(port_on_coastline, width = 3)
  len[k]             <-   rgeos::gLength(crop(coastline10, port_on_coastline))
  
  k <- k+1
  print(k/length(ports@data$INDEX_NO))
}


## Combining the data 
port_df <- cbind(mean_slope, 
                 mean_tri, 
                 min_elev,
                 max_elev, 
                 mean_elev,
                 elevation_p,
                 tri_p,
                 slope_p,
                 cont,
                 len,
                 river_dist,
                 y)


sample <- spsample(coastline10, 
                   5000, 
                   type = "random", 
                   method="Line")
sample <- sample[sample(length(sample)),] 
sample <- sample[1:dim(ports@data)[1]]

## Sampled locations 
# Buffer around each point
a <- gBuffer(sample, width = bwidth, byid=TRUE)

# Elevation in buffer
b            <-  raster::extract(elev, a)
tri_sample   <-  raster::extract(elev, a)
slope_sample <-  raster::extract(slope, a)

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

# Adding continent
cont <- over(a, countries10)$SUBREGION
cont <- ifelse(is.na(cont), "na", cont)

# Measuring distance to river
river_dist <- sapply(1:length(a@polygons), 
                     function(i)gDistance(a[i], rivers50))

# Adding information on the coastline length
len <- sapply(1:length(a@polygons), 
              function(i) rgeos::gLength(crop(coastline10, a[i])))


## Combining the data 
no_port_df <- cbind(mean_slope, 
                    mean_tri, 
                    min_elev,
                    max_elev, 
                    mean_elev,
                    elevation_p,
                    tri_p,
                    slope_p,
                    cont,
                    len,
                    river_dist,
                    y)

## Generating dataframe for prediction
pred_dataframe <- rbind(port_df, no_port_df) %>% 
  data.frame() %>%
  mutate(y=as.factor(y),
         mean_slope=as.numeric(as.character(mean_slope)), 
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
names <- colnames(pred_dataframe)[1:11]
for(i in 2:5) { 
  
  k <- sapply(1:11, function(x) pred_dataframe[,x]^i) 
  
  colnames(k) <- paste(names[1:11], i, sep = "")
  
  d <- cbind(d, k)
}
polynomials <- d %>% as.data.frame() %>%  
  dplyr::select(-c(cont2, cont3, cont4, cont5))


## Adding interaction terms
inter1           <- sapply(2:11, 
                           function(x) pred_dataframe[,1]*pred_dataframe[,x])
colnames(inter1) <- paste0("mean_slope", colnames(pred_dataframe)[2:11])

inter2           <- sapply(3:11, 
                           function(x) pred_dataframe[,2]*pred_dataframe[,x])
colnames(inter2) <- paste0("mean_tri", colnames(pred_dataframe)[3:11])

inter3           <- sapply(4:11, 
                           function(x) pred_dataframe[,3]*pred_dataframe[,x])
colnames(inter3) <- paste0("min_elev", colnames(pred_dataframe)[4:11])

inter4           <- sapply(5:11, 
                           function(x) pred_dataframe[,4]*pred_dataframe[,x])
colnames(inter4) <- paste0("max_elev", colnames(pred_dataframe)[5:11])

inter5           <- sapply(6:11, 
                           function(x) pred_dataframe[,5]*pred_dataframe[,x])
colnames(inter5) <- paste0("mean_elev", colnames(pred_dataframe)[6:11])

inter6           <- sapply(7:11, 
                           function(x) pred_dataframe[,6]*pred_dataframe[,x])
colnames(inter6) <- paste0("elevation_p", colnames(pred_dataframe)[7:11])

inter7           <- sapply(8:11, 
                           function(x) pred_dataframe[,7]*pred_dataframe[,x])
colnames(inter7) <- paste0("tri_p", colnames(pred_dataframe)[8:11])

inter8           <- sapply(11:11, 
                           function(x) pred_dataframe[,11]*pred_dataframe[,x])
colnames(inter8) <- paste0("len", colnames(pred_dataframe)[11:11])


interactions <- cbind(inter1, inter2, inter3, inter4, inter5, 
                       inter6, inter7, inter8) %>% 
                as.data.frame() %>% 
                dplyr::select(-c(mean_tricont, tri_pcont, elevation_pcont, 
                                 mean_elevcont, max_elevcont, min_elevcont, 
                                 mean_tricont, mean_slopecont))

## Adding dummy variables
dummies         <-  model.matrix(~pred_dataframe$cont) %>% as.data.frame()

## Joining the datasets
pred_dataframe1 <- cbind(pred_dataframe, polynomials, interactions, dummies)%>% 
  filter(!is.na(slope_p))

## Resampling the order of the rows
pred_dataframe1 <- pred_dataframe1[sample(nrow(pred_dataframe1)),] 

