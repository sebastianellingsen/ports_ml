## Description: 
## This file prepares a basic shapefile consisting of a grid covering Spanish 
## America. The grid-cell size is 0.5 degrees. Basic geographic information 
## is also added to the shapefile.

## Loading datasets 
set.seed(11042018)

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

study_area  <- countries110[countries110@data$ADMIN %in% countries_list,]
audiencia   <- readOGR("data/audiencia_viceroyalty/Audiencia_Dissolve_1790/Audiencia_Dissolve_1790.shp", 
                       "Audiencia_Dissolve_1790")
viceroyalty <- readOGR("data/audiencia_viceroyalty/Virreinato_Dissolve_1790/Virreinato_Dissolve_1790.shp", 
                       "Virreinato_Dissolve_1790")

## Loading shapefiles on states and provinces  by country
file_names <- list.files('data/regions')

loading_shapefiles <- function(x){
  path      <- paste('data/regions/', x, sep = '')
  shapefile <- paste(x, '.shp', sep = '')
  full_path <- paste(path, shapefile, sep = '/')
  return(readOGR(full_path, x))
}
states <- bind(sapply(file_names, function(x) loading_shapefiles(x)))

## Subsetting and adding the same projection 
study_area  <- study_area[study_area@data$TYPE%in%c("Sovereign country"), ]
states      <- spTransform(states, crs(study_area))
states_list <- unique(states@data$ADMIN_NAME)


## Generating the grid cells
hex_points <- spsample(study_area, 
                       type = "hexagonal", 
                       cellsize = 0.5)

hexagons <- sapply(1:nrow(hex_points@coords), 
                   function(x) HexPoints2SpatialPolygons(hex_points[x],
                                                         dx = 0.5))
hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)

dataframe <- sapply(hexagons@polygons, 
                        function(x) x@ID) %>% data.frame() %>% 
  rename('ID' ='.') %>% 
  set_rownames(.$ID)

south_america <- SpatialPolygonsDataFrame(hexagons, 
                                          dataframe, 
                                          match.ID = TRUE)

## Adding country fixed effects
country_fe <- function(x){
  cell <- south_america[south_america@data$ID == x, ]
  return(over(cell, study_area)$ADMIN)
}
south_america@data$ccode <- sapply(south_america@data$ID, 
                                   function(x) country_fe(x))

## Adding state fixed effects
state_fe <- function(x){
  cell <- south_america[south_america@data$ID == x, ]
  return(over(cell, states)$ADMIN_NAME)
}
south_america@data$state <- sapply(south_america@data$ID, 
                                        function(x) state_fe(x))


## Viceroyalty fixed effects  
viceroyalty_fe <- function(x){
  cell <- south_america[south_america@data$ID==x, ]
  return(over(cell, viceroyalty)$Nombre)
}
south_america@data$viceroyalty <- as.character(sapply(south_america@data$ID, 
                                                 function(x) viceroyalty_fe(x)))

## Audiencia fixed effects  
audiencia_fe <- function(x){
  cell <- south_america[south_america@data$ID==x, ]
  return(over(cell, audiencia)$Nombre)
}
south_america@data$audiencia <- as.character(sapply(south_america@data$ID, 
                                                function(x) audiencia_fe(x)))


# Southern Venezuela missing, adding these manually 
south_america@data$viceroyalty <- ifelse(is.na(south_america@data$viceroyalty), 
                                         'Nueva Granada', 
                                         south_america@data$viceroyalty)

south_america@data$audiencia <- ifelse(is.na(south_america@data$audiencia), 
                                       'frontier', 
                                       south_america@data$audiencia)

# Longitude and latitude 
south_america@data$lon <- coordinates(south_america)[,1]
south_america@data$lat <- coordinates(south_america)[,2]



