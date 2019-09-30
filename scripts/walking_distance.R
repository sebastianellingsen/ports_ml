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
ports <- spTransform(ports, crs(elev_sa))


## Calculate walking distance to lima for each grid cell 
altDiff <- function(x){x[2] - x[1]} 
hd      <- transition(elev_sa, altDiff, 8, symm=FALSE)

slope   <- geoCorrection(hd)

adj         <- adjacent(elev_sa, 
                        cells=1:ncell(elev_sa), 
                        pairs=TRUE, 
                        directions=8)
speed       <- slope
speed[adj]  <- 0.5 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)

walking_path <- function(x, port){
  cell <- centroids[x]
  return(costDistance(Conductance, port, cell)/36000)
}

## Calculating the distances

# Main ports
w_dist_lima       <- sapply(1:length(centroids), 
                           function(x) walking_path(x, 
                                                    ports[ports@data$ports=="Lima",]))
w_dist_veracruz   <- sapply(1:length(centroids), 
                           function(x) walking_path(x, 
                                                    ports[ports@data$ports=="Veracruz",]))
w_dist_cartagena  <- sapply(1:length(centroids), 
                         function(x) walking_path(x, 
                                                  ports[ports@data$ports=="Cartagena",]))
w_dist_panama     <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Panama",]))
w_dist_acapulco   <- sapply(1:length(centroids), 
                         function(x) walking_path(x, 
                                                  ports[ports@data$ports=="Acapulco",]))

# Secondary ports
w_dist_manta          <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Manta",]))
w_dist_esmeraldas     <- sapply(1:length(centroids), 
                             function(x) walking_path(x, 
                                                      ports[ports@data$ports=="Esmeraldas",]))
w_dist_trujillo       <- sapply(1:length(centroids), 
                             function(x) walking_path(x, 
                                                      ports[ports@data$ports=="Trujillo",]))
w_dist_huacho         <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Huacho",]))
w_dist_paita          <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Paita",]))
w_dist_huarmey        <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Huarmey",]))
w_dist_maldonado      <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Maldonado",]))
w_dist_carupano       <- sapply(1:length(centroids), 
                             function(x) walking_path(x, 
                                                      ports[ports@data$ports=="Carupano",]))
w_dist_barcelona      <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Barcelona",]))
w_dist_puertocabello  <- sapply(1:length(centroids), 
                             function(x) walking_path(x, 
                                                      ports[ports@data$ports=="Puerto Cabello",]))
w_dist_maracaibo      <- sapply(1:length(centroids), 
                                 function(x) walking_path(x, 
                                                          ports[ports@data$ports=="Maracaibo",]))
w_dist_barranquilla   <- sapply(1:length(centroids), 
                             function(x) walking_path(x, 
                                                      ports[ports@data$ports=="Barranquilla",]))
w_dist_buenaventura   <- sapply(1:length(centroids), 
                              function(x) walking_path(x, 
                                                       ports[ports@data$ports=="Buenaventura",]))
w_dist_portobelo      <- sapply(1:length(centroids), 
                               function(x) walking_path(x, 
                                                        ports[ports@data$ports=="Portobelo",]))
w_dist_puntarena      <- sapply(1:length(centroids), 
                              function(x) walking_path(x, 
                                                       ports[ports@data$ports=="Puntarenas",]))
w_dist_elrealejo      <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="El Realejo",]))
w_dist_tela           <- sapply(1:length(centroids), 
                            function(x) walking_path(x, 
                                                     ports[ports@data$ports=="Tela",]))
w_dist_campeche       <- sapply(1:length(centroids), 
                                  function(x) walking_path(x, 
                                                           ports[ports@data$ports=="Campeche",]))
w_dist_tuxpan         <- sapply(1:length(centroids), 
                                   function(x) walking_path(x, 
                                                            ports[ports@data$ports=="Tuxpan",]))
w_dist_manzanillo     <- sapply(1:length(centroids), 
                                    function(x) walking_path(x, 
                                                             ports[ports@data$ports=="Mazanillo",]))
w_dist_santodomingo   <- sapply(1:length(centroids), 
                                    function(x) walking_path(x, 
                                                             ports[ports@data$ports=="Santo Domingo",]))
w_dist_santiagodecuba <- sapply(1:length(centroids), 
                                    function(x) walking_path(x, 
                                                             ports[ports@data$ports=="Santiago de Cuba",]))
w_dist_havana         <- sapply(1:length(centroids), 
                                    function(x) walking_path(x, 
                                                             ports[ports@data$ports=="Havana",]))
w_dist_st_castilla    <- sapply(1:length(centroids), 
                                function(x) walking_path(x, 
                                                         ports[ports@data$ports=="Santo Tomas de Castilla",]))
w_dist_omoa           <- sapply(1:length(centroids), 
                                function(x) walking_path(x, 
                                                         ports[ports@data$ports=="Omoa",]))
w_dist_chagres        <- sapply(1:length(centroids), 
                              function(x) walking_path(x, 
                                                       ports[ports@data$ports=="Chagres",]))
w_dist_riohacha       <- sapply(1:length(centroids), 
                                 function(x) walking_path(x, 
                                                          ports[ports@data$ports=="Riohacha",]))
w_dist_caracas        <- sapply(1:length(centroids), 
                                  function(x) walking_path(x, 
                                                           ports[ports@data$ports=="Caracas",]))
w_dist_valparaiso     <- sapply(1:length(centroids), 
                                 function(x) walking_path(x, 
                                                          ports[ports@data$ports=="Valparaiso",]))
w_dist_concepcion     <- sapply(1:length(centroids), 
                                    function(x) walking_path(x, 
                                                             ports[ports@data$ports=="Concepcion",]))
w_dist_arica          <- sapply(1:length(centroids), 
                                    function(x) walking_path(x, 
                                                             ports[ports@data$ports=="Arica",]))
w_dist_guyayaquil     <- sapply(1:length(centroids), 
                               function(x) walking_path(x, 
                                                        ports[ports@data$ports=="Guyayaquil",]))
w_dist_montevideo     <- sapply(1:length(centroids), 
                                function(x) walking_path(x, 
                                                         ports[ports@data$ports=="Montevideo",]))
w_dist_buenos_aires     <- sapply(1:length(centroids), 
                                function(x) walking_path(x, 
                                                         ports[ports@data$ports=="Buenos Aires",]))

## Joining the full dataset
df1 <- cbind(ID=south_america_m@data$ID, w_dist_lima,
                                         w_dist_veracruz,
                                         w_dist_cartagena,
                                         w_dist_panama,
                                         w_dist_acapulco,
                                         w_dist_manta,
                                         w_dist_esmeraldas,
                                         w_dist_trujillo,
                                         w_dist_huacho,
                                         w_dist_paita,
                                         w_dist_huarmey,
                                         w_dist_maldonado,
                                         w_dist_carupano,
                                         w_dist_barcelona,
                                         w_dist_puertocabello,
                                         w_dist_maracaibo,
                                         w_dist_barranquilla,
                                         w_dist_buenaventura,
                                         w_dist_portobelo,
                                         w_dist_puntarena,
                                         w_dist_elrealejo,
                                         w_dist_tela,
                                         w_dist_tuxpan,
                                         w_dist_manzanillo,
                                         w_dist_santodomingo,
                                         w_dist_santiagodecuba,
                                         w_dist_havana,
                                         w_dist_st_castilla,
                                         w_dist_omoa,
                                         w_dist_chagres,
                                         w_dist_riohacha,
                                         w_dist_caracas,
                                         w_dist_valparaiso,
                                         w_dist_concepcion,
                                         w_dist_arica,
                                         w_dist_guyayaquil,
                                         w_dist_campeche,
                                         w_dist_montevideo,
                                         w_dist_buenos_aires) %>% as.data.frame() 

df2 <- cbind(ID=south_america@data$ID) %>% 
  as.data.frame() %>%  full_join(df1, by="ID") %>% 
  dplyr::select(w_dist_lima,
                w_dist_veracruz,
                w_dist_cartagena,
                w_dist_panama,
                w_dist_acapulco,
                w_dist_manta,
                w_dist_esmeraldas,
                w_dist_trujillo,
                w_dist_huacho,
                w_dist_paita,
                w_dist_huarmey,
                w_dist_maldonado,
                w_dist_carupano,
                w_dist_barcelona,
                w_dist_puertocabello,
                w_dist_maracaibo,
                w_dist_barranquilla,
                w_dist_buenaventura,
                w_dist_portobelo,
                w_dist_puntarena,
                w_dist_elrealejo,
                w_dist_tela,
                w_dist_tuxpan,
                w_dist_manzanillo,
                w_dist_santodomingo,
                w_dist_santiagodecuba,
                w_dist_havana,
                w_dist_st_castilla,
                w_dist_omoa,
                w_dist_chagres,
                w_dist_riohacha,
                w_dist_caracas,
                w_dist_valparaiso,
                w_dist_concepcion,
                w_dist_arica,
                w_dist_guyayaquil,
                w_dist_campeche,
                w_dist_montevideo,
                w_dist_buenos_aires)

south_america@data$wd_lima             <- df2$w_dist_lima
south_america@data$wd_veracruz         <- df2$w_dist_veracruz
south_america@data$wd_cartagena        <- df2$w_dist_cartagena
south_america@data$wd_panama           <- df2$w_dist_panama
south_america@data$wd_acapulco         <- df2$w_dist_acapulco
south_america@data$wd_manta            <- df2$w_dist_manta
south_america@data$wd_esmeraldas       <- df2$w_dist_esmeraldas
south_america@data$wd_trujillo         <- df2$w_dist_trujillo
south_america@data$wd_huacho           <- df2$w_dist_huacho
south_america@data$wd_paita            <- df2$w_dist_paita
south_america@data$wd_huarmey          <- df2$w_dist_huarmey
south_america@data$wd_maldonado        <- df2$w_dist_maldonado
south_america@data$wd_carupano         <- df2$w_dist_carupano
south_america@data$wd_barcelona        <- df2$w_dist_barcelona
south_america@data$wd_puertocabello    <- df2$w_dist_puertocabello
south_america@data$wd_maracaibo        <- df2$w_dist_maracaibo
south_america@data$wd_barranquilla     <- df2$w_dist_barranquilla
south_america@data$wd_buenaventura     <- df2$w_dist_buenaventura
south_america@data$wd_portobelo        <- df2$w_dist_portobelo
south_america@data$wd_puntarena        <- df2$w_dist_puntarena
south_america@data$wd_elrealejo        <- df2$w_dist_elrealejo
south_america@data$wd_tela             <- df2$w_dist_tela
south_america@data$wd_tuxpan           <- df2$w_dist_tuxpan
south_america@data$wd_manzanillo       <- df2$w_dist_manzanillo
south_america@data$wd_santodomingo     <- df2$w_dist_santodomingo
south_america@data$wd_santiagodecuba   <- df2$w_dist_santiagodecuba
south_america@data$wd_havana           <- df2$w_dist_havana
south_america@data$wd_st_castilla      <- df2$w_dist_st_castilla
south_america@data$wd_omoa             <- df2$w_dist_omoa
south_america@data$wd_chagres          <- df2$w_dist_chagres
south_america@data$wd_riohacha         <- df2$w_dist_riohacha
south_america@data$wd_caracas          <- df2$w_dist_caracas
south_america@data$wd_valparaiso       <- df2$w_dist_valparaiso
south_america@data$wd_concepcion       <- df2$w_dist_concepcion
south_america@data$wd_arica            <- df2$w_dist_arica
south_america@data$wd_guyayaquil       <- df2$w_dist_guyayaquil
south_america@data$wd_campeche         <- df2$w_dist_campeche
south_america@data$wd_montevideo       <- df2$w_dist_montevideo
south_america@data$wd_buenos_aires     <- df2$w_dist_buenos_aires

