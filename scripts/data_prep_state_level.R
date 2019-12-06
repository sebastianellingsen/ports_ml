
# Loading data 
file_path <- c('states/geo1_ar1970_2010/geo1_ar1970_2010.shp',
               'states/geo1_bo1976_2001/geo1_bo1976_2001.shp',
               'states/geo1_cl1982_2002/geo1_cl1982_2002.shp',
               'states/geo1_co1964_2005/geo1_co1964_2005.shp',
               'states/geo1_cr1963_2011/geo1_cr1963_2011.shp',
               'states/geo1_cu2002_2002/geo1_cu2002_2002.shp',
               'states/geo1_do1960_2010/geo1_do1960_2010.shp',
               'states/geo1_ec1962_2010/geo1_ec1962_2010.shp',
               'states/geo1_gt1964_2002/geo1_gt1964_2002.shp',
               'states/geo1_hn1961_2001/geo1_hn1961_2001.shp',
               'states/geo1_mx1960_2015/geo1_mx1960_2015.shp',
               'states/geo1_ni1971_2005/geo1_ni1971_2005.shp',
               'states/geo1_pa1960_2010/geo1_pa1960_2010.shp',
               'states/geo1_pe1993_2007/geo1_pe1993_2007.shp',
               'states/geo1_py1962_2002/geo1_py1962_2002.shp',
               'states/geo1_sv1992_2007/geo1_sv1992_2007.shp',
               'states/geo1_uy1963_2011/geo1_uy1963_2011.shp',
               'states/geo1_ve1971_2001/geo1_ve1971_2001.shp')
path <- file_path[1]
loading_states <- function(path){
  states <- readOGR(path, substr(path, 25, nchar(path)-4))
  states <- spTransform(states, crs(south_america))
  return(states)
}
states <- sapply(file_path, function(x) loading_states(x))
states <- bind(states)


## Aggregating data by state 

## Measuring the consentration along the coastline 
## Nightlights in coastal areas
coast_by_state <- function(x){
  state <- states[rownames(states@data) == x,]
  df      <- over(state, south_america, returnList = T)[[1]]
  coastal <- sum(df[df$coastal == 1,]$night_lights, na.rm = T)
  total   <- sum(df$night_lights, na.rm = T)
  return(coastal/total)
}
states@data$coast_night_lights <- sapply(rownames(states@data), 
                                         function(x) coast_by_state(x))

## Nightlights in coastal areas
coast_by_state <- function(x){
  state <- states[rownames(states@data) == x,]
  df      <- over(state, south_america, returnList = T)[[1]]
  coastal <- sum(df[df$coastal == 1,]$pop, na.rm = T)
  total   <- sum(df$pop, na.rm = T)
  return(coastal/total)
}
states@data$coast_pop <- sapply(rownames(states@data), 
                                function(x) coast_by_state(x))


## Find change in market access for each state
access_by_state <- function(x, var){
  state <- states[rownames(states@data) == x,]
  return(mean(over(state, south_america, returnList = T)[[1]][[var]], 
              na.rm=T))
}

# Travel time 
states@data$ma <- sapply(rownames(states@data), 
                         function(x) access_by_state(x, 'log_d'))

# Mineral deposits
states@data$mine <- sapply(rownames(states@data), 
                         function(x) access_by_state(x, 'mine'))

# Cotton suitability
states@data$cotton <- sapply(rownames(states@data), 
                                 function(x) access_by_state(x, 'cotton'))

# Cotton suitability
states@data$coffee <- sapply(rownames(states@data), 
                             function(x) access_by_state(x, 'coffee'))

# Banana suitability
states@data$banana <- sapply(rownames(states@data), 
                             function(x) access_by_state(x, 'banana'))

# Terrain ruggedness
states@data$tri <- sapply(rownames(states@data), 
                             function(x) access_by_state(x, 'tri'))

# Slope
states@data$slope <- sapply(rownames(states@data), 
                          function(x) access_by_state(x, 'slope'))

# Temperature
states@data$bio1 <- sapply(rownames(states@data), 
                            function(x) access_by_state(x, 'bio1'))

# Precipitation
states@data$bio12 <- sapply(rownames(states@data), 
                           function(x) access_by_state(x, 'bio12'))

# Share of cells in a coastal area 
states@data$coastal <- sapply(rownames(states@data), 
                              function(x) access_by_state(x, 'coastal'))

## Mean distance to historical port
states@data$dist_port_1777 <- sapply(rownames(states@data), 
                                     function(x) access_by_state(x, 'dist_port_1777'))

## Mean population
states@data$pop <- sapply(rownames(states@data), 
                          function(x) access_by_state(x, 'pop'))

## Mean elevation
states@data$elev <- sapply(rownames(states@data), 
                           function(x) access_by_state(x, 'elev'))

## Mean distance to coastline
states@data$coast_ds <- sapply(rownames(states@data), 
                               function(x) access_by_state(x, 'coast_ds'))

## Finding the audiencia and viceroyalty
access_by_state <- function(x, var){
  state <- states[rownames(states@data) == x,]
  return(over(state, south_america)[[var]])
}
states@data$audiencia <- sapply(rownames(states@data), 
                                   function(x) access_by_state(x, 'audiencia'))
states@data$viceroyalty <- sapply(rownames(states@data), 
                                function(x) access_by_state(x, 'viceroyalty'))

states_level <- states@data %>% 
           mutate(ma1 = ifelse(ma>1.032, 1, 0)) %>% 
           dplyr::rename(country = CNTRY_NAME, 
                         state = ADMIN_NAME) %>% 
           filter(!is.na(ma))




## Merging with data from Maloney and Caicedo 
# id <- read_csv('states/data.csv') %>%
#   dplyr::full_join(data, by = c('country', 'state')) %>%
#   mutate(id = as.character(id)) %>% 
#   dplyr::full_join(df, by = c('country', 'state')) %>% 
#   filter(!is.na(id)) 
#   
# rownames(id) <- id$id
# 
# states1       <- states[rownames(states@data) %in% id$id, ]
# states1       <- SpatialPolygonsDataFrame(states1, id, match.ID = TRUE)

# m1 <- felm(data = df, 
#            coast_night_lights~ma1 + coastal +dist_port_1777|
#            country|
#            0|
#            state)







