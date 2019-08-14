## This file prepares africa grid cell file

## Loading packages and datasets 
set.seed(11042018)

crs_south_america <- "+proj=moll +datum=WGS84 +units=km +pm=bogota"

countries_list <- c("Chile", "Bolivia", "Peru", "Argentina", "Brazil", "Uruguay",   
                  "Ecuador", "Colombia", "Paraguay", "Venezuela", "Panama",
                  "El Salvador", "Honduras", "Costa Rica", "Guatemala", 
                  "Mexico", "Nicaragua", "Cuba", "Dominican Republic")

study_area <- countries10[countries10@data$ADMIN %in% countries_list, ]
study_area_unprojected <- countries110[countries110@data$ADMIN %in% 
                                         countries_list ,]

# Project 
study_area <- study_area[study_area@data$TYPE%in%c("Sovereign country"), ]
study_area <- spTransform(study_area, 
                          crs_south_america)

## Generating the grid cells
buffer <- gBuffer(study_area, width = 50)

hex_points <- spsample(buffer, 
                       type = "hexagonal", 
                       cellsize = 50)

hexagons <- sapply(1:nrow(hex_points@coords), 
                   function(x) HexPoints2SpatialPolygons(hex_points[x],
                                                         dx = 50))

hexagons <- list(hexagons, makeUniqueIDs = TRUE) %>% 
  flatten() %>% 
  do.call(rbind, .)


## Adding country fixed effects
ID_country <- c()
country_var <- c()
continent_var <- c()
income_group_var <- c()
country_logical <- rep(NA, length(hexagons@polygons))

for (j in countries_list){
  
  country <- study_area[study_area$ADMIN==j,]
  
  country_logical <- gIntersects(country, 
                                 hexagons,
                                 byid=TRUE)
  
  # A country level dataset is generated and values are extracted from 
  # the natural earth dataset
  country_tmp <- hexagons[country_logical]
  ID_country_tmp <- sapply(country_tmp@polygons, 
                           function(x) x@ID)
  country_var_tmp <- rep(country@data$NAME, 
                         length(ID_country_tmp))
  income_group_var_tmp <- rep(country@data$INCOME_GRP, 
                              length(ID_country_tmp))
  continent_var_tmp <- rep(country@data$CONTINENT, 
                           length(ID_country_tmp))
  
  # This data are added to vectors storing this information 
  ID_country <- c(ID_country, 
                  ID_country_tmp)
  country_var <- c(country_var,
                   country_var_tmp)
  income_group_var <- c(income_group_var,
                        income_group_var_tmp)
  continent_var <- c(continent_var,
                     continent_var_tmp)
  
  print(j)
}


## Preparing the files to join
ID_country_vector <- unlist(ID_country)
country_df <- data.frame(ID=ID_country_vector, 
                         ccode=country_var)

country_df <- country_df  %>% 
  distinct(ID, .keep_all = TRUE)

row.names(country_df) <- country_df$ID

# Cells that are in the country dataset
final <- hexagons[sapply(hexagons@polygons, 
                        function(x) x@ID) %in% country_df$ID]

# Adding a dataframe to the grid cells 
south_america <- SpatialPolygonsDataFrame(final, 
                                          country_df, 
                                          match.ID = TRUE)

# Output: south_america is an sp file of grid cells containing basic country 
#         level data.



