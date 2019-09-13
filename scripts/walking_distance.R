## This script calculates the walking distance from each port to each grid-cell

library("gdistance")

countries_list <- c("Chile", 
                    "Bolivia", 
                    "Peru", 
                    "Brazil",
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
                    "Dominican Republic",
                    "United States of America")

## Reproject and find the centroid of each grid-cell
south_america_m <- spTransform(south_america, 
                               "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +pm=bogota +units=m +no_defs")
centroids <- gCentroid(south_america_m, byid = TRUE)

elev               <- raster("data/elevation/ETOPO1_Ice_g_geotiff.tif")
crs(elev)          <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
countries110       <- ne_download(scale = 110, 
                                  type = 'countries', 
                                  category = 'cultural')
sa                 <- countries110[countries110@data$ADMIN %in% countries_list,]
sa                 <- gBuffer(sa, width = 0.1)
elev_sa            <- raster::crop(elev, sa)
elev_sa            <- raster::mask(elev_sa, sa)
elev_sa            <- aggregate(elev_sa, 40)
elev_sa            <- projectRaster(elev_sa, 
                                    crs = crs(south_america_m), 
                                    method = "bilinear")
elev_sa[is.na(elev_sa)] <- -99999

# Ports in the Spanish Empire
source("scripts/data_prep_ports.R")
se_ports <- spTransform(ports, crs(elev_sa))


## Calculate walking distance to lima for each grid cell 
altDiff <- function(x){x[2] - x[1]} 
hd      <- transition(elev_sa, altDiff, 8, symm=FALSE)

slope   <- geoCorrection(hd)

adj         <- adjacent(elev_sa, 
                        cells=1:ncell(elev_sa), 
                        pairs=TRUE, 
                        directions=8)
speed       <- slope
speed[adj]  <- 1.5 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)


# cell <- centroids[100]
# AtoB <- shortestPath(Conductance, lima, cell, output="SpatialLines")

walking_path <- function(x, port){
  cell <- centroids[x]
  return(costDistance(Conductance, port, cell)/36000)
}

## Calculating the distances
w_dist_lima       <- sapply(1:length(centroids), 
                           function(x) walking_path(x, se_ports[3]))
w_dist_veracruz   <- sapply(1:length(centroids), 
                           function(x) walking_path(x, se_ports[1]))
w_dist_cartagena  <- sapply(1:length(centroids), 
                         function(x) walking_path(x, se_ports[2]))
w_dist_panama     <- sapply(1:length(centroids), 
                            function(x) walking_path(x, se_ports[4]))
w_dist_acapulco   <- sapply(1:length(centroids), 
                         function(x) walking_path(x, se_ports[5]))

## Joining the full dataset
df1 <- cbind(ID=south_america_m@data$ID, w_dist_lima, 
                                         w_dist_veracruz,
                                         w_dist_cartagena,
                                         w_dist_panama,
                                         w_dist_acapulco) %>% 
  as.data.frame() 

df2 <- cbind(ID=south_america@data$ID) %>% 
  as.data.frame() %>%  full_join(df1, by="ID") %>% 
  dplyr::select(w_dist_lima, 
                w_dist_veracruz, 
                w_dist_cartagena, 
                w_dist_panama, 
                w_dist_acapulco)

south_america@data$wd_lima      <- df2$w_dist_lima
south_america@data$wd_veracruz  <- df2$w_dist_veracruz
south_america@data$wd_cartagena <- df2$w_dist_cartagena
south_america@data$wd_panama    <- df2$w_dist_panama
south_america@data$wd_acapulco  <- df2$w_dist_acapulco


south_america@data <- south_america@data %>% mutate(walk=log(pmin(wd_lima,
                                                wd_veracruz,
                                                wd_cartagena,
                                                wd_panama,
                                                wd_acapulco)))

