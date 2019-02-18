
##############################################
## Generating  dataset at the country level ##
##############################################

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rgeos, viridis, ranger, tmaptools)

#hexagons_full <- gIntersection(hexagons, study_area, byid = TRUE)

model <- ranger(formula= as.numeric(y)~., data=training_data, num.trees = 4000, mtry = 5)
prediction <- predict(model, dataset)$predictions

dataset_final <- dataset %>%  dplyr::select(y) %>% mutate(y_pred=prediction)
row.names(dataset_final) <- row.names(data)

sps_df <- SpatialPolygonsDataFrame(hexagons, dataset_final, match.ID = TRUE)



## Generating country level dataset
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


#######################################
## Generating the country level data ##
#######################################

if (!require(pacman)) install.packages("pacman")
p_load(readxl, naniar, countrycode)

# Loading data 
excel_sheets("data/mpd2018.xlsx")
excel_sheets("data/Trade_of_Goods.xlsx")

# Trade 
trade_data <- read_excel("data/Trade_of_Goods.xlsx", skip=5) %>% 
  dplyr::select(-"Base Year", -"Scale") %>% 
  replace_with_na_all(condition=~.x=="...") %>% 
  slice(1:185) %>% 
  gather("year", "trade",2:167) %>% 
  filter(year==2010, !is.na(trade)) %>% 
  mutate(trade=as.numeric(trade), year=as.numeric(year))

country_code <- sapply(trade_data$Country, 
                       function(x) countrycode(x, 'country.name', 'iso3c'))
trade_data$country_code <- country_code 
trade_data <- trade_data[!is.na(trade_data$country_code),]

# PWT data
econ_data <- read_excel("data/pwt90.xlsx", sheet="Data") %>% 
  filter(year==2010) %>% 
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
  dplyr::select(scode, country, year, polity2, democ) %>% filter(year==2010) 
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











# Nightlights
dataset <- data_pred %>%
  mutate_all(type.convert) %>%
  mutate_if(is.factor, as.numeric) 

dataset[is.na(dataset)] <- 0
dataset$y <- as.factor(dataset$y)

model <- ranger(formula= as.numeric(y)~., data=training_data, num.trees = 4000, mtry = 5)
prediction <- predict(model, dataset)$predictions

dataset_final <- dataset %>%  dplyr::select(y) %>% mutate(y_pred=prediction)
row.names(dataset_final) <- coast_data_final[,1]

# add the country it belongs to 
lights <- raster("data/nightlights/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")

sps_df_coastal <- SpatialPolygonsDataFrame(coast_hexagons, dataset_final, match.ID = TRUE)

lights_small <- aggregate(lights, 6)
lights_small_projected <- projectRaster(lights_small, crs = newcrs)

lights_data <- rep(NA, length(coast_hexagons@polygons))
for (i in 1:length(coast_hexagons@polygons)){
  lights_data[i] <- mean(values(crop(lights_small_projected, coast_hexagons[i])), na.rm=TRUE)
  print(i)
}

sps_df_coastal$lights_data <- lights_data

lights_reg_data <-  sps_df_coastal@data
summary(lm(data=lights_reg_data_1, formula=y_pred~lights_data))

lights_reg_data_1 <- lights_reg_data %>% 
  mutate(lights_data=log(1+lights_data), y_pred=log(y_pred)) 

ggplot(data=lights_reg_data_1, aes(x=y_pred, y=lights_data))+
  stat_summary_bin(fun.y='mean', bins=200,color='blue',alpha=0.3, size=2, geom='point')+
  geom_rug(alpha = 0.01) + xlab("") + ylab("")+ggtitle("Nightlights and port suitability")






# In what country are the hexagons?

countries <- countries_list@data$ADMIN

ID_country <- c()
country_var <- c()
country_logical <- rep(NA, length(coast_hexagons@polygons))

#countries <- c("Denmark", "Germany", "Brazil")

for (j in countries){
  
  country <- countries10[countries10$ADMIN==j,]
  
  for (i in 1:length(coast_hexagons@polygons)){
    country_logical[i] <- gIntersects(coast_hexagons[i], country)==TRUE
    
    print(c(i/length(coast_hexagons@polygons),country@data$NAME))
  }
  
  country_tmp <- coast_hexagons[country_logical]
  ID_country_tmp <- sapply(country_tmp@polygons, function(x) x@ID)
  country_var_tmp <- rep(country@data$NAME, length(ID_country_tmp))
  
  ID_country <- c(ID_country, ID_country_tmp)
  country_var <- c(country_var,country_var_tmp)

}

# sa sjekk at man kan joine, lag en ny sa du ikke ma loade igjen
ID_country_vector <- unlist(ID_country)
country_df <- data.frame(ID_country_vector, country_var)

country_df <- country_df  %>% 
  distinct(ID_country_vector, .keep_all = TRUE)

row.names(country_df) <- country_df$ID_country_vector

final<- coast_hexagons[sapply(coast_hexagons@polygons, function(x) x@ID) %in% country_df$ID_country]

final_pdf<- SpatialPolygonsDataFrame(final, 
                                     country_df, match.ID = TRUE)


plot(final_pdf[final_pdf@data$country_var=="Indonesia",])




sps_df_coastal_df <- sps_df_coastal@data
sps_df_coastal_df$ID <- row.names(sps_df_coastal_df)
final_pdf@data$ID <- final_pdf@data$ID_country_vector
final_pdf_df <- final_pdf@data

sps_df_coastal_df_tomatch <- sps_df_coastal_df[row.names(sps_df_coastal_df)%in%final_pdf_df$ID,]

lights_fe <- sps_df_coastal_df_tomatch %>% full_join(final_pdf_df,by="ID")


summary(lm(data=lights_fe, formula=y_pred~lights_data + factor(country_var)))



# Population density 


# add the country it belongs to 
pop_density <- raster("data/population_density/gpw-v4-population-density-rev10_2005_2pt5_min_tif/gpw_v4_population_density_rev10_2005_2pt5_min.tif")

sps_df_coastal <- SpatialPolygonsDataFrame(coast_hexagons, dataset_final, match.ID = TRUE)


pop_density_projected <- projectRaster(pop_density, crs = newcrs)

lights_data <- rep(NA, length(coast_hexagons@polygons))
for (i in 1:length(coast_hexagons@polygons)){
  lights_data[i] <- mean(values(crop(lights_small_projected, coast_hexagons[i])), na.rm=TRUE)
  print(i)
}

sps_df_coastal$lights_data <- lights_data





  
  
  



# Share of gpd
commodity_data <- read_excel("data/API_TX.VAL.MMTL.ZS.UN_DS2_en_excel_v2_10404372.xls", skip=3) %>% 
  dplyr::select(-"Indicator Name", -"Indicator Code", -"Country Code") %>% 
  gather("year", "share", 2:60) %>% 
  filter(year>=1997, !is.na(share)) 




## Commodity prices
commodity_data <- read_excel("data/CMOHistoricalDataAnnual.xlsx", sheet="Annual Prices (Real)",skip=8) %>% 
  dplyr::select(X__1,KSILVER,KPLATINUM,KGOLD,KZinc,KNICKEL,KTin,KLEAD,
                KCOPPER,KIRON_ORE,KALUMINUM,KPOTASH) %>% 
  rename(year=X__1, Silver=KSILVER,Platinum=KPLATINUM,Gold=KGOLD,
         Zinc=KZinc,Nickel=KNICKEL,Tin=KTin,Lead=KLEAD,Copper=KCOPPER,
         Iron=KIRON_ORE,Aluminum=KALUMINUM,Potash=KPOTASH) %>% 
  filter(year>=1990, year<=2015) %>% 
  gather("year metal", "price", 2:12) 
  
colnames(commodity_data)[2] <- "metal"
commodity_data$metal <- as.factor(commodity_data$metal)

commodity_data <- commodity_data %>% 
  group_by(metal) %>% 
  mutate(price_1990=ifelse(year==1990,price,0)) %>% 
  mutate(price_1990=max(price_1990)) %>% 
  ungroup() %>% 
  mutate(price=price/price_1990)

ggplot(data=commodity_data, aes(x=year, y=price,color=metal))+geom_line(alpha=0.9) +
  xlab("") + ylab("") +
  ggtitle("Prices (normalized)") +
  geom_vline(xintercept=2003, linetype = "longdash",alpha=0.6)+
  theme(legend.title = element_blank()) 




lead og potash mangler
## Commodity production

metals <- c("aluminum", "copper", "gold", "iron_ore", "nickel", "platinum",
            "silver", "tin", "zinc")

for (m in metals){
  path <- paste(paste("data/commodities/production/",m,sep = "")
                ,"/statisticsExport (1).xlsx" ,sep = "")
  #metal <- read_excel(path, skip=1)
  print(path)
}


metal <- read_excel("data/commodities/production/iron_ore/statisticsExport (2).xlsx", skip=1) %>% 
  rename("2001"=X__1,"2002"=X__2,"2003"=X__3,"2004"=X__4,"2005"=X__5,"2006"=X__6,
         "2007"=X__7,"2008"=X__8,"2009"=X__9,"2010"=X__10, "country"="\n\tCountry") %>% 
  dplyr::select(country, "2001","2002","2003","2004","2005","2006","2007","2008",
                "2009","2010") %>% 
  slice(1:55) %>% 
  gather("country year", "tonnes", 2:11) %>% 
  rename("year"="country year") %>% 
  filter(!is.na(tonnes)) %>% 
  mutate(country=as.factor(country),year=as.numeric(year))
  
metal_normalized <- metal %>% 
  group_by(country) %>% 
  mutate(tonnes_1990=ifelse(year==2001,tonnes,0)) %>% 
  mutate(tonnes_1990=max(tonnes_1990)) %>% 
  ungroup() %>% 
  mutate(tonnes=tonnes/tonnes_1990)




ggplot(data=metal_normalized, aes(x=year, y=tonnes,color=country))+geom_line() +
  xlab("") + ylab("") +
  ggtitle("Prices (normalized)") +
  geom_vline(xintercept=2003, linetype = "longdash",alpha=0.6)+
  theme(legend.title = element_blank()) 

