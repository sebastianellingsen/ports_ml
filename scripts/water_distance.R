## This file loads files and function in order to calculate the distance from 
## each port to Cadiz and the primary port in each region 

library("gdistance")
library("rWind")

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")

wind_series_mean_layer <- stack("output/wind_series_mean_layer.grd")
Conductance <- flow.dispersion(wind_series_mean_layer, 
                               output="transitionLayer", 
                               type="active")

## Preparing dataset on ports
source("scripts/data_prep_ports.R")
ports       <- spTransform(ports, crs(wind_series_mean_layer))
ports_spain <- spTransform(ports_spain, crs(wind_series_mean_layer))

# Primary ports
cadiz     <- ports_spain[ports_spain@data$ports=="Cadiz", ]
veracruz  <- ports[ports@data$ports=="Veracruz", ]
panama    <- ports[ports@data$ports=="Panama", ]
cartagena <- ports[ports@data$ports=="Cartagena", ]
lima      <- ports[ports@data$ports=="Lima", ]
portobelo <- ports[ports@data$ports=="Portobelo", ]

# Secondary ports 
manta                   <- ports[ports@data$ports=="Manta", ]
guyayaquil              <- ports[ports@data$ports=="Guyayaquil", ]
esmeraldas              <- ports[ports@data$ports=="Esmeraldas", ]
trujillo                <- ports[ports@data$ports=="Trujillo", ]
huacho                  <- ports[ports@data$ports=="Huacho", ]
paita                   <- ports[ports@data$ports=="Paita", ]
huarmey                 <- ports[ports@data$ports=="Huarmey", ]
arica                   <- ports[ports@data$ports=="Arica", ]
concepcion              <- ports[ports@data$ports=="Concepcion", ]
valparaiso              <- ports[ports@data$ports=="Valparaiso", ]
buenos_aires            <- ports[ports@data$ports=="Buenos Aires", ]
montevideo              <- ports[ports@data$ports=="Montevideo", ]
maldonado               <- ports[ports@data$ports=="Maldonado", ]
carupano                <- ports[ports@data$ports=="Carupano", ]
barcelona               <- ports[ports@data$ports=="Barcelona", ]
caracas                 <- ports[ports@data$ports=="Caracas", ]
puerto_cabello          <- ports[ports@data$ports=="Puerto Cabello", ]
maracaibo               <- ports[ports@data$ports=="Maracaibo", ]
barranquilla            <- ports[ports@data$ports=="Barranquilla", ]
buenaventura            <- ports[ports@data$ports=="Buenaventura", ]
riohacha                <- ports[ports@data$ports=="Riohacha", ]
portobelo               <- ports[ports@data$ports=="Portobelo", ]
chargres                <- ports[ports@data$ports=="Chagres", ]
puntarenas              <- ports[ports@data$ports=="Puntarenas", ]
el_realejo              <- ports[ports@data$ports=="El Realejo", ]
tela                    <- ports[ports@data$ports=="Tela", ]
omoa                    <- ports[ports@data$ports=="Omoa", ]
santo_tomas_de_castilla <- ports[ports@data$ports=="Santo Tomas de Castilla", ]
campeche                <- ports[ports@data$ports=="Campeche", ]
veracruz                <- ports[ports@data$ports=="Veracruz", ]
tuxpan                  <- ports[ports@data$ports=="Tuxpan", ]
mazanillo               <- ports[ports@data$ports=="Mazanillo", ]
santo_domingo           <- ports[ports@data$ports=="Santo Domingo", ]
havana                  <- ports[ports@data$ports=="Havana", ]
santiago_de_cuba        <- ports[ports@data$ports=="Santiago de Cuba", ]

# Clean and load the logbook file  
crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
logbook <- read.csv("data/cliwoc/logbook.csv", stringsAsFactors = F) 

logbook <- logbook %>% filter(!is.na(cost), cost!="Inf", cost!="0") %>% 
  filter(time_diff==1) %>% 
  mutate(cost = as.numeric(cost)/dist, speed = dist/time_diff) %>% 
  filter(dist<500, cost<1)

# Fit the model 
m1 <- lm(data = logbook, speed ~ cost)

# Function for calculating the sailing time 
travel_time <- function(path){
  
  # Store the points as spatial points
  coords <- path@lines[[1]]@Lines[[1]]@coords
  path_points <- SpatialPoints(coords = coords, 
                               proj4string = crs(crs))
  
  # Calculate the conductance cost of travelling 
  travel_costs <- lapply(2:(length(path_points)-1), 
                         function(i) costDistance(Conductance, coords[i+1,], coords[i,])) %>% 
    unlist()
  
  # Calculate the distance travelled  
  path_points_proj <- spTransform(path_points, newcrs)
  travel_distance <- lapply(2:(length(path_points)-1), 
                            function(i) gDistance(path_points_proj[i+1], 
                                                  path_points_proj[i])) %>% 
    unlist()
  
  # Store the results and fit the model
  df <- cbind(travel_costs, travel_distance) %>% as.data.frame() %>% 
    mutate(cost = travel_costs/travel_distance)
  df$fitted <- predict(m1, df)
  
  ttime <- sum(1/df$fitted*50)
  
  return(ttime)
} 

## From main ports to Cadiz 
primary_ports <- c(veracruz, cartagena, portobelo)
travel_cadiz <- function(x){
  path <- shortestPath(Conductance, x, cadiz, output = "SpatialLines")
  time <- travel_time(path)
  print(x@data$ports)
  return(time)
}
travel_time_cadiz <- sapply(primary_ports, function(x) travel_cadiz(x))  


## Calculating the shortest path through each port pre-period
source("scripts/distance_calc/primary_ports/water_distance_lima.R")
source("scripts/distance_calc/primary_ports/water_distance_cartagena.R")
source("scripts/distance_calc/primary_ports/water_distance_panama.R")
source("scripts/distance_calc/primary_ports/water_distance_veracruz.R")
source("scripts/distance_calc/primary_ports/water_distance_islands.R")

## Calculating the shortest path through each port post-period
source("scripts/distance_calc/secondary_ports/water_distance_arica.R")
source("scripts/distance_calc/secondary_ports/water_distance_buenos_aires.R")
source("scripts/distance_calc/secondary_ports/water_distance_campeche.R")
source("scripts/distance_calc/secondary_ports/water_distance_caracas.R")
source("scripts/distance_calc/secondary_ports/water_distance_chagres.R")
source("scripts/distance_calc/secondary_ports/water_distance_concepcion.R")
source("scripts/distance_calc/secondary_ports/water_distance_guyayaquil.R")
source("scripts/distance_calc/secondary_ports/water_distance_islands.R")
source("scripts/distance_calc/secondary_ports/water_distance_montevideo.R")
source("scripts/distance_calc/secondary_ports/water_distance_omoa.R")
source("scripts/distance_calc/secondary_ports/water_distance_riohacha.R")
source("scripts/distance_calc/secondary_ports/water_distance_std_castilla.R")
source("scripts/distance_calc/secondary_ports/water_distance_valparaiso.R")




# alle turene tar 5 dager ekstra for reformen, men mange slipper dette etter,
# dette skaper gevinster for ports som blir internasjonale i tillegg til de
# som er der allerede. to gevinster: slipper omvei om annen port,
# slipper turn tilbake
# veien gjennom port i tar fem dager, den forsvinner for noen etter (post)
# ta med dette





# see inni hvert land om det na funker bedre 
# fikse buenos aires og montevideo 

## Preparing the final data
final <- south_america@data %>% 
  mutate(walk_pre = pmin(walk_cartagena, 
                         walk_panama,
                         walk_veracruz, 
                         walk_cuba,
                         walk_domrep,
                         walk_lima),
         walk_post = pmin(walk_campeche, 
                          walk_st_castilla,
                          walk_omoa, 
                          walk_chargres,
                          walk_caracas,
                          walk_riohacha,
                          walk_montevideo,
                          walk_buenos_aires,
                          walk_valparaiso,
                          walk_concepcion,
                          walk_arica,
                          walk_guyayaquil,
                          walk_cartagena, 
                          walk_panama,
                          walk_veracruz, 
                          walk_cuba,
                          walk_domrep,
                          walk_lima))

south_america@data$walk_pre  <- final$walk_pre
south_america@data$walk_post <- final$walk_post
south_america@data$log_d <- 1/(final$walk_post/final$walk_pre)


