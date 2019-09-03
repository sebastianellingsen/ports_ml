## This file prepares africa grid cell file

## Loading packages and datasets 
set.seed(11042018)

crs_south_america <- "+proj=moll +datum=WGS84 +units=km +pm=bogota"

countries_list <- c("Chile", 
                    "Bolivia", 
                    "Peru", 
                    "Argentina", 
                    "Brazil", 
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

study_area <- countries110[countries110@data$ADMIN %in% countries_list, ]
study_area_unprojected <- countries110[countries110@data$ADMIN %in% 
                                       countries_list ,]


## States and provinces IPUMS
argentina           <- readOGR("data/regions/geo1_ar1970_2010/geo1_ar1970_2010.shp",
                               "geo1_ar1970_2010") 
bolivia             <- readOGR("data/regions/geo1_bo1976_2001/geo1_bo1976_2001.shp",
                               "geo1_bo1976_2001") 
brazil              <- readOGR("data/regions/geo1_br1960_2010/geo1_br1960_2010.shp",
                               "geo1_br1960_2010") 
chile               <- readOGR("data/regions/geo1alt_cl1960_1970/geo1alt_cl1960_1970.shp",
                               "geo1alt_cl1960_1970") 
colombia            <- readOGR("data/regions/geo1_co1964_2005/geo1_co1964_2005.shp",
                               "geo1_co1964_2005") 
costa_rica          <- readOGR("data/regions/geo1_cr1963_2011/geo1_cr1963_2011.shp",
                               "geo1_cr1963_2011") 
cuba                <- readOGR("data/regions/geo1_cu2002_2002/geo1_cu2002_2002.shp",
                               "geo1_cu2002_2002") 
dominican_republic  <- readOGR("data/regions/geo1_do1960_2010/geo1_do1960_2010.shp",
                               "geo1_do1960_2010") 
ecuador             <- readOGR("data/regions/geo1_ec1962_2010/geo1_ec1962_2010.shp",
                               "geo1_ec1962_2010") 
el_salvador         <- readOGR("data/regions/geo1_sv1992_2007/geo1_sv1992_2007.shp",
                               "geo1_sv1992_2007") 
guatemala           <- readOGR("data/regions/geo1_gt1964_2002/geo1_gt1964_2002.shp",
                               "geo1_gt1964_2002") 
honduras            <- readOGR("data/regions/geo1_hn1961_2001/geo1_hn1961_2001.shp",
                               "geo1_hn1961_2001") 
mexico              <- readOGR("data/regions/geo1_mx1960_2015/geo1_mx1960_2015.shp",
                               "geo1_mx1960_2015") 
nicaragua           <- readOGR("data/regions/geo1_ni1971_2005/geo1_ni1971_2005.shp",
                               "geo1_ni1971_2005") 
panama              <- readOGR("data/regions/geo1_pa1960_2010/geo1_pa1960_2010.shp",
                               "geo1_pa1960_2010")
paraguay            <- readOGR("data/regions/geo1_py1962_2002/geo1_py1962_2002.shp",
                               "geo1_py1962_2002") 
peru                <- readOGR("data/regions/geo1_pe1993_2007/geo1_pe1993_2007.shp",
                               "geo1_pe1993_2007") 
venezuela           <- readOGR("data/regions/geo1_ve1971_2001/geo1_ve1971_2001.shp",
                               "geo1_ve1971_2001") 
uruguay             <- readOGR("data/regions/geo1_uy1963_2011/geo1_uy1963_2011.shp",
                               "geo1_uy1963_2011") 


states <- bind(argentina,  bolivia,  brazil,             chile,     colombia, 
               costa_rica, cuba,     dominican_republic, ecuador,   el_salvador, 
               guatemala,  honduras, mexico,             nicaragua, panama, 
               paraguay,   uruguay,  peru,               venezuela)

states      <- spTransform(states, crs_south_america)
states_list <- unique(states@data$ADMIN_NAME)

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

  # This data are added to vectors storing this information 
  ID_country <- c(ID_country, 
                  ID_country_tmp)
  country_var <- c(country_var,
                   country_var_tmp)

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

# Adding the state
fe <- c()
k <- 1
for(i in south_america@data$ID){
  cell <- south_america[south_america@data$ID==i, ]
  fe[k] <- as.character(over(cell, states)$ADMIN_NAME)
  k <- k+1
  print(i)
}
south_america@data$states <- fe

# Replacing missing states
south_america@data$states <- ifelse(south_america@data$ID=="ID1286",
                                    "Aysen", 
                                    south_america@data$states)
south_america@data$states <- ifelse(south_america@data$ID=="ID1303",
                                    "Aysen", 
                                    south_america@data$states)
south_america@data$states <- ifelse(south_america@data$ID=="ID1875",
                                    "Buenos Aires province", 
                                    south_america@data$states)
south_america@data$states <- ifelse(south_america@data$ID=="ID14367",
                                    "Bahia", 
                                    south_america@data$states)
south_america@data$states <- ifelse(south_america@data$ID=="ID17875",
                                    "Amapá", 
                                    south_america@data$states)
south_america@data$states <- ifelse(south_america@data$ID=="ID18599",
                                    "Antioquia", 
                                    south_america@data$states)

south_america@data$states <- as.factor(south_america@data$states)
# Output: south_america is an sp file of grid cells containing basic country 
#         level data.


