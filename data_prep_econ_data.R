
##############################################
## Generating  dataset at the country level ##
##############################################

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rgeos, viridis, 
       ranger, tmaptools, readxl, naniar, countrycode)

#hexagons_full <- gIntersection(hexagons, study_area, byid = TRUE)

model <- ranger(formula= as.numeric(y)~., 
                data=training_data, num.trees = 4000, mtry = 5)
prediction <- predict(model, dataset)$predictions

dataset_final <- dataset %>%  dplyr::select(y) %>% mutate(y_pred=prediction)
row.names(dataset_final) <- row.names(data)

sps_df <- SpatialPolygonsDataFrame(hexagons, dataset_final, match.ID = TRUE)


##############################################
## Generating the within country level data ##
#############################################

# Preparing coast data 
dataset <- data_pred %>%
  mutate_all(type.convert) %>%
  mutate_if(is.factor, as.numeric) 

dataset[is.na(dataset)] <- 0
dataset$y <- as.factor(dataset$y)

model <- ranger(formula= as.numeric(y)~., data=training_data, 
                num.trees = 4000, mtry = 5)
prediction <- predict(model, dataset)$predictions

dataset_final <- dataset %>%  dplyr::select(y) %>% mutate(y_pred=prediction)
row.names(dataset_final) <- coast_data_final[,1]

sps_df_coastal <- SpatialPolygonsDataFrame(coast_hexagons, dataset_final, 
                                           match.ID = TRUE)

# Loading, projecting and aggregating raster files 
lights <- raster("data/nightlights/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")
lights_small <- aggregate(lights, 2)
lights_small_projected <- projectRaster(lights_small, crs = newcrs, method = "bilinear")

pop_density <- raster("data/population_density/gpw-v4-population-density-rev10_2005_30_sec_tif/gpw_v4_population_density_rev10_2005_30_sec.tif")
crs(pop_density) <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=")
pop_density_projected <- projectRaster(pop_density, crs = newcrs, method = "bilinear")

africa_polis <- readOGR("data/population_density/Africapolis_2015_shp/Africapolis.shp", "Africapolis")


# problemet er at jeg bruker veldig hoy resolution, last ned litt laver

# Extracting the values
lights_data <- rep(NA, length(coast_hexagons@polygons))
density_data <- rep(NA, length(coast_hexagons@polygons))

for (i in 1:length(coast_hexagons@polygons)){
  lights_data[i] <- mean(values(crop(lights_small_projected, 
                                     coast_hexagons[i])), na.rm=TRUE)
  print(i)
}

# Joining and preparing datasets
sps_df_coastal$lights_data <- lights_data
sps_df_coastal$density_data <- pop_density_data

## Generating country fixed effects
countries <- countries_list@data$ADMIN

ID_country <- c()
country_var <- c()
continent_var <- c()
income_group_var <- c()
country_logical <- rep(NA, length(coast_hexagons@polygons))

## This section adds fixed effects 

# defining the sample


for (j in countries_list1@data$ADMIN){
  
  country <- countries10[countries10$ADMIN==j,]
  
  for (i in 1:length(coast_hexagons@polygons)){
    country_logical[i] <- gIntersects(coast_hexagons[i], country)==TRUE
    
    print(c(i/length(coast_hexagons@polygons),country@data$NAME))
  }
  
  country_tmp <- coast_hexagons[country_logical]
  ID_country_tmp <- sapply(country_tmp@polygons, function(x) x@ID)
  country_var_tmp <- rep(country@data$NAME, length(ID_country_tmp))
  income_group_var_tmp <- rep(country@data$INCOME_GRP, length(ID_country_tmp))
  continent_var_tmp <- rep(country@data$CONTINENT, length(ID_country_tmp))
  
  ID_country <- c(ID_country, ID_country_tmp)
  country_var <- c(country_var,country_var_tmp)
  income_group_var <- c(income_group_var,income_group_var_tmp)
  continent_var <- c(continent_var,continent_var_tmp)
}


## Joining the data
ID_country_vector <- unlist(ID_country)
country_df <- data.frame(ID_country_vector, country_var, 
                         income_group_var, continent_var)

country_df <- country_df  %>% 
  distinct(ID_country_vector, .keep_all = TRUE)

row.names(country_df) <- country_df$ID_country_vector

final<- coast_hexagons[sapply(coast_hexagons@polygons, 
                              function(x) x@ID) %in% country_df$ID_country]

final_pdf<- SpatialPolygonsDataFrame(final, 
                                     country_df, match.ID = TRUE)

sps_df_coastal_df <- sps_df_coastal@data
sps_df_coastal_df$ID <- row.names(sps_df_coastal_df)
final_pdf@data$ID <- final_pdf@data$ID_country_vector
final_pdf_df <- final_pdf@data

sps_df_coastal_df_tomatch <- sps_df_coastal_df[row.names(sps_df_coastal_df)%in%final_pdf_df$ID,]

coastal_data_fe <- sps_df_coastal_df_tomatch %>% 
  full_join(final_pdf_df,by="ID") %>% 
  mutate(y_p=ifelse(y_pred>=1.65,1,0)) %>% 
  filter(continent_var!="Europe", continent_var!="Oceania",
         continent_var=="Africa")


###########################################################
## Adding country and continent information to port data ##
###########################################################
ports_full <- readOGR("data/WPI_Shapefiles/WPI_Shapefile2010", "WPI")
ports_full <- spTransform(ports_full, newcrs)
ports_full@data$nr <- rep(1:3718)
           
africa <- countries_list[countries_list@data$CONTINENT=="Africa",]
buffer <- gBuffer(africa, width = 40)
ports_logical <- c()

for (j in ports_full@data$nr){
  port_tmp <- ports_full[ports_full@data$nr==j,]
  ports_logical[j] <- gIntersects(port_tmp, buffer)==TRUE
}
ports_africa <- ports_full[ports_logical,]
ports_africa_cn <- ports_africa[ports_africa@data$HARBORSIZE!="V",]

# Final dataset of larger ports
non_ssa <- c("EG", "LY","IS", "SP", "TS", "AG", "SU", "GI", "MO")
ports_africa_cn <- ports_africa@data %>% 
  filter(!(COUNTRY %in% non_ssa), HARBORSIZE!="V")






#######################################
## Generating the country level data ##
#######################################

ssa <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
         "Cape Verde", "Central African Republic", "Chad", "Congo, Democratic Republic of", 
         "Congo, Republic of", "Cote div", "Côte d'Ivoire", "Gambia, The",
         "Djibouti", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon",
         "The Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho",
         "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius",
         "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda",
         "Senegal", "Sierra Leone", "Uganda", "Western Sahara", "Zambia", 
         "Somalia", "South Africa", "Sudan", "Swaziland", "Tanzania", "Togo",
         "Zimbabwe", "Eswatini, Kingdom of")

countries_list <- countries10[countries10$TYPE=="Sovereign country"|countries10$TYPE=="Country",]

n_harbors <- rep(0, nrow(countries_list@data))
harbors <- rep(0, nrow(countries_list@data))

for (i in countries_list@data$SOVEREIGNT){
  study_area_country <- countries_list[countries_list$SOVEREIGNT==i,]
  dataset_country <- gIntersection(sps_df, study_area_country, byid = TRUE)
  row.names(dataset_country) <- gsub("\\s.*", "", sapply(dataset_country@polygons, function(x) x@ID))
  sps_df_temp <- sps_df[row.names(sps_df) %in% sapply(dataset_country@polygons, function(x) x@ID), ]
  
  n_harbors[which(countries_list@data$SOVEREIGNT==i)] <- sum(ifelse(sps_df_temp@data$y_pred>1.6,1,0))
  harbors[which(countries_list@data$SOVEREIGNT==i)] <- sum(as.numeric(sps_df_temp@data$y)-1)
  
  print(c(i, n_harbors[which(countries_list@data$SOVEREIGNT==i)], harbors[which(countries_list@data$SOVEREIGNT==i)]))
}

countries_list@data$n_harbors <- n_harbors
countries_list@data$harbors <- harbors
countries_list@data$c_area <- area(countries_list)

# Coordinated in the current CRS
long <- rep(0, nrow(countries_list@data))
lat <- rep(0, nrow(countries_list@data))

for (i in countries_list@data$SOVEREIGNT){
  long[which(countries_list@data$SOVEREIGNT==i)] <- extent(countries_list[countries_list$SOVEREIGNT==i,])[1]
  lat[which(countries_list@data$SOVEREIGNT==i)] <- extent(countries_list[countries_list$SOVEREIGNT==i,])[3]
}

countries_list@data$long <- long
countries_list@data$lat <- lat

## Adding controls

# Loading data 
excel_sheets("data/mpd2018.xlsx")
excel_sheets("data/Trade_of_Goods.xlsx")

# Trade 
trade_data <- read_excel("data/Trade_of_Goods.xlsx", skip=5) %>% 
  dplyr::select(-"Base Year", -"Scale") %>% 
  replace_with_na_all(condition=~.x=="...") %>% 
  slice(1:185) %>% 
  gather("year", "trade",2:167) %>% 
  filter(year<1970,
         !is.na(trade), Country %in% ssa) %>% 
  mutate(trade=as.numeric(trade), year=as.numeric(year)) %>% 
  group_by(Country) %>% 
  summarise(trade=mean(trade))

country_code <- sapply(trade_data$Country, 
                       function(x) countrycode(x, 'country.name', 'iso3c'))
trade_data$country_code <- country_code 
trade_data <- trade_data[!is.na(trade_data$country_code),]

# PWT data
econ_data <- read_excel("data/pwt90.xlsx", sheet="Data") %>% 
  filter(year==1960) %>% 
  rename(country_code=countrycode)

# Urban population
urban_data <- read_excel("data/urban_population.xls", sheet="Data", skip=3) %>% 
  rename(urban="2010", country_code="Country Code") %>% 
  dplyr::select(country_code, urban)

# Data on ports and harbors
region <- c("MAC","HKG","GRL","ALA","CUW","SXM","ABW","JEY","GGY","IMN")
harbor_data <- countries_list@data %>% 
  mutate(country=SOVEREIGNT, country_code=ISO_A3, continent=CONTINENT) %>% 
  dplyr::select(n_harbors, harbors, country, c_area, country_code, continent, long, lat) %>% 
  filter(!(country_code %in% region), country!="Northern Cyprus", country!="Kosovo")

harbor_data[which(harbor_data$country=="Norway"),5] <- "NOR"
harbor_data[which(harbor_data$country=="France"),5] <- "FRA"

# Polity iv 
polity_data <- read_excel("data/p4v2017.xls") %>% 
  dplyr::select(scode, country, year, polity2, democ) %>% 
  filter(year>=1960 & year<=1970) %>% 
  group_by(country) %>% 
  summarise(polity=mean(polity2))
polity_data$country_code <- sapply(polity_data$country, 
                                   function(x) countrycode(x, 'country.name', 'iso3c'))

# Coastline:
coastline_data <- read_excel("data/coastline.xlsx", col_names=c("country", "length"))
coastline_data$country_code <- sapply(coastline_data$country, 
                                      function(x) countrycode(x, 'country.name', 'iso3c'))
coastline_data <- coastline_data %>% filter(!is.na(country_code))

# Combining datasets
combined <- inner_join(econ_data, harbor_data, by = "country_code") %>% 
  inner_join(polity_data, by = c("country_code")) %>% 
  inner_join(coastline_data, by = c("country_code")) %>%
  inner_join(urban_data, by = c("country_code")) %>% 
  filter(length>0) 







##########################################
## Generating the grid cell level data ###
##########################################

# defining the sample
countries_list1 <- countries_list[countries_list@data$CONTINENT=="Africa",]

countries <-  countries_list1@data %>% 
  filter(!(ADMIN %in% c("Libya","Egypt", "Tunisia", "Algeria", "Morocco")))

ID_country <- c()
country_var <- c()
continent_var <- c()
income_group_var <- c()
country_logical <- rep(NA, length(hexagons@polygons))

for (j in countries$ADMIN){
  
  country <- countries10[countries10$ADMIN==j,]
  
  for (i in 1:length(hexagons@polygons)){
    country_logical[i] <- gIntersects(hexagons[i], country)==TRUE
    
    print(c(i/length(hexagons@polygons),country@data$NAME))
  }
  
  country_tmp <- hexagons[country_logical]
  ID_country_tmp <- sapply(country_tmp@polygons, function(x) x@ID)
  country_var_tmp <- rep(country@data$NAME, length(ID_country_tmp))
  income_group_var_tmp <- rep(country@data$INCOME_GRP, length(ID_country_tmp))
  continent_var_tmp <- rep(country@data$CONTINENT, length(ID_country_tmp))
  
  ID_country <- c(ID_country, ID_country_tmp)
  country_var <- c(country_var,country_var_tmp)
  income_group_var <- c(income_group_var,income_group_var_tmp)
  continent_var <- c(continent_var,continent_var_tmp)
}

# Preparing the files to join
ID_country_vector <- unlist(ID_country)
country_df <- data.frame(ID_country_vector, country_var, 
                         income_group_var, continent_var)

country_df <- country_df  %>% 
  distinct(ID_country_vector, .keep_all = TRUE)

row.names(country_df) <- country_df$ID_country_vector
final<- hexagons[sapply(hexagons@polygons, 
                              function(x) x@ID) %in% country_df$ID_country]
final_pdf<- SpatialPolygonsDataFrame(final, 
                                     country_df, match.ID = TRUE)@data

final_pdf$ID <- row.names(final_pdf)
africa_tmp <- sps_df[row.names(sps_df@data)%in%row.names(final_pdf),]@data
africa_tmp$ID <- row.names(africa_tmp)

## Joining the data
africa_df <- africa_tmp %>% full_join(final_pdf,by="ID")
row.names(africa_df) <- africa_df$ID

final_to_match <- sps_df[row.names(sps_df@data)%in%row.names(final_pdf),]
africa <- SpatialPolygonsDataFrame(final_to_match,
                                   africa_df, match.ID = TRUE)
                                 



## Measuring distance between grid cells
distances_predicted <- c()
distances_actual <- c()
distance_coast <- c()
africa@data$yp <- ifelse(africa@data$y_pred>=1.65, 1, 0)
#ports_1 <- ports[!is.na(ports@data$HARBORSIZE),]
#ports_1 <- ports_1[ports_1@data$HARBORSIZE!="V",]

port_cell_actual <- africa[africa@data$y==1,]
port_cell_predicted <- africa[africa@data$yp==1,]
counter <- 0
for (i in row.names(africa@data)){

  cell <- africa[africa@data$ID==i,]
  distances_actual[i] <- gDistance(cell, ports)
  distances_predicted[i] <- gDistance(cell, port_cell_predicted)
  distance_coast[i] <- gDistance(cell, coastline10)
  
  counter <- counter+1
  print(counter/length(row.names(africa@data)))
}

africa@data$distances_predicted <- log(distances_predicted+1)
africa@data$distances_actual <- log(distances_actual+1)
africa@data$distance_coast <- log(distance_coast+1)
africa@data$distance_coast2 <- log(distance_coast+1)^2
africa@data$distance_coast3 <- log(distance_coast+1)^3




## Extracting data from rasters

library(spatialEco)

# Loading, projecting and aggregating raster files 
lights <- raster("data/nightlights/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")
lights_small <- aggregate(lights, 2)
lights_small_projected <- projectRaster(lights_small, crs = newcrs, method = "bilinear")

pop_density <- raster("data/population_density/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev10_2015_2pt5_min_tif/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev10_2015_2pt5_min.tif")
pop_density <- aggregate(pop_density, 2)
pop_density_projected <- projectRaster(pop_density, crs = newcrs, method = "bilinear")

# Extracting the values
lights_data <- c()
density_data <- c()
tri <- c()
counter <- 0

for (i in row.names(africa@data)){
  cell <- africa[africa@data$ID==i,]
  lights_data[i] <- mean(values(crop(lights_small_projected, 
                                     cell)), na.rm=TRUE)
  density_data[i] <- mean(values(crop(pop_density_projected, 
                                     cell)), na.rm=TRUE)
  ri <- tri(crop(elev, cell), exact = TRUE, s = 3)
  tri[i] <- mean(values(ri),na.rm=TRUE)
  
  counter <- counter+1
  print(counter/length(row.names(africa@data)))
}

# Joining and preparing datasets

africa@data$lights_data <- log(lights_data+1)
africa@data$density_data <- log(density_data+1)
africa@data$tri <- tri

africa@data$long <- coordinates(africa)[,1]
africa@data$lat <- coordinates(africa)[,2]



africa@data$coast <- ifelse(distance_coast<=100,1,0)



kenya <- africa[africa@data$country_var=="Mozambique"|
                africa@data$country_var=="Malawi"|
                africa@data$country_var=="Zambia"|
                africa@data$country_var=="Zimbabwe"|
                africa@data$country_var=="South Africa"|
                africa@data$country_var=="Swasiland"|
                africa@data$country_var=="Tanzania",]

p1 <- tm_shape(kenya) +  
  tm_fill(col="coast", palette=plasma(256),n=15) + 
  tm_layout(frame=TRUE, legend.show=FALSE,bg.color="grey85") 

p2 <- tm_shape(kenya) +  
  tm_fill(col="distances_actual", palette=plasma(256),n=15) + 
  tm_layout(frame=TRUE, legend.show=FALSE,bg.color="grey85") 

p3 <- tm_shape(kenya) +  
  tm_fill(col="distances_predicted", palette=plasma(256),n=15) + 
  tm_layout(frame=TRUE, legend.show=FALSE,bg.color="grey85") 

print(tmap_arrange(p1,p2,p3))

#save(africa,file="africa.Rda")
summary(lm(data=africa, distances_actual~distances_predicted+factor(country_var)))




## Generating city level data 
africa_polis <- readOGR("data/population_density/Africapolis_2015_shp/Africapolis.shp", "Africapolis")
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
africa_polis <- spTransform(africa_polis, newcrs)

coastline10 <- ne_download(scale = 110, 
                           type = 'coastline', category = 'physical')
coastline10 <- spTransform(coastline10, newcrs)

port_cell_actual <- africa[africa@data$y==1,]
port_cell_predicted <- africa[africa@data$yp==1,]
distance_predicted <- c()
distance_actual <- c()
distance_coast <- c()
counter <- 0

for (i in 1:nrow(africa_polis@data)){
  id <- africa_polis@data[i,1]
  cell <- africa_polis[africa_polis@data$ID==id,]
  distance_predicted[i] <- gDistance(port_cell_predicted, cell)
  distance_actual[i] <- gDistance(port_cell_actual, cell)
  distance_coast[i] <- gDistance(coastline10, cell)

  counter <- counter+1
  print(counter/length(row.names(africa_polis@data)))
}

africa_polis@data$distance_predicted <- distance_predicted
africa_polis@data$distance_actual <- distance_actual
africa_polis@data$distance_coast <- distance_coast

landlocked <- c("ZWE", "BWA", "BFA", "BDI", "TCD", "ETH", "LSO", "MWI", "MLI", 
                "NER", "RWA", "CAF", 
                "SDS", "UGA", "ZMB", "ZWE")

africa_cities <- africa_polis@data %>% 
  filter(!(ISO %in% c("MAR", "EGY", "TUN", "LBY", "DZA"))) %>% 
  mutate(pop2010=(as.numeric(as.character(pop2010))),
         pop2000=(as.numeric(as.character(pop2000))),
         pop1970=(as.numeric(as.character(pop1970))),
         pop1950=(as.numeric(as.character(pop1950))),
         pop1980=(as.numeric(as.character(pop1980))),
         Dens2015=(as.numeric(as.character(Dens2015))),
         pop1990=(as.numeric(as.character(pop1990))),
         pop1960=as.numeric(as.character(pop1960)))

%>% 

    mutate(g=log(pop2010-pop1960),
         g1970=log(pop1970-pop1960),
         g1980=log(pop1980-pop1970),
         g1990=log(pop1990-pop1980),
         g2000=log(pop2000-pop1990),
         g2010=log(pop2010-pop2000),
         d2=log(1+distance_coast)^2,
         d3=log(1+distance_coast)^3) %>% 
  filter(is.finite(g)) 


#save(africa,file="africa_roads.Rda")

# Joining africa cities with trade data
africa_cities_trade <- africa_cities %>% rename(country_code=ISO) %>% 
  full_join(trade_data, by="country_code") %>% 
  filter(!is.na(Name)) %>% 
  mutate(distance_coast2 = distance_coast^2)

# Joining africa cities with democracy data
africa_cities_polity <- africa_cities %>% rename(country_code=ISO) %>% 
  full_join(polity_data, by="country_code") %>% 
  filter(!is.na(polity), !is.na(distance_actual))





















mozambique <- africa[africa@data$country_var=="Mozambique",]

tm_shape(mozambique) +  
  tm_fill(col="distances_road", palette=plasma(256),n=40) + 
  tm_layout(frame=TRUE, legend.show=FALSE,bg.color="white", 
            main.title="Min. distance to paved road", main.title.size=0.7) 




## Landcover data
land_cover <- raster("data/landcover/Globcover2009_V2.3_Global_/GLOBCOVER_L4_200901_200912_V2.3.tif")
land_cover_small <- aggregate(land_cover, 4)
water_areas <- land_cover[land_cover==210]




















