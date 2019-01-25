

##############################################
## Generating  dataset at the country level ##
##############################################

if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rgeos, viridis, ranger, tmaptools)

## Setting theme
theme_set(theme_bw() + theme(panel.grid.minor = element_line(colour = "white", size = 0.5),
                             panel.grid.major = element_line(colour = "white", size = 0.2)))

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
  select(-"Base Year", -"Scale") %>% 
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

# Data on ports and harbors
region <- c("MAC","HKG","GRL","ALA","CUW","SXM","ABW","JEY","GGY","IMN")
harbor_data <- countries_list@data %>% 
  mutate(country=SOVEREIGNT, country_code=ISO_A3, continent=CONTINENT) %>% 
  select(n_harbors, harbors, country, c_area, country_code, continent) %>% 
  filter(!(country_code %in% region), country!="Northern Cyprus", country!="Kosovo")

harbor_data[which(harbor_data$country=="Norway"),5] <- "NOR"
harbor_data[which(harbor_data$country=="France"),5] <- "FRA"

# Polity iv 
polity_data <- read_excel("data/p4v2017.xls") %>% 
  select(scode, country, year, polity2, democ) %>% filter(year==2010) 
polity_data$country_code <- sapply(polity_data$country, 
        function(x) countrycode(x, 'country.name', 'iso3c'))

# Landlocked countries:
landlocked <- c(
  "Afghanistan",	
  "Andorra",
  "Armenia",
  "Austria",	
  "Azerbaijan",
  "Belarus",	
  "Bhutan",	
  "Bolivia",	
  "Botswana",	
  "Burkina Faso",	
  "Burundi",	
  "Central African Republic",	
  "Chad",
  "Czech Republic",	
  "Eswatini", 
  "Ethiopia",
  "Hungary",	
  "Kazakhstan",
  "Kosovo",
  "Kyrgyzstan",	
  "Laos",	
  "Lesotho",
  "Liechtenstein",	
  "Luxembourg",
  "Macedonia",	
  "Malawi",	
  "Mali",	
  "Moldova",	
  "Mongolia",	
  "Nepal",	
  "Niger",	
  "Paraguay",	
  "Rwanda",	
  "San Marino",	
  "Serbia",	
  "Slovakia",	
  "South Ossetia",
  "South Sudan",
  "Switzerland",	
  "Tajikistan",	
  "Transnistria",
  "Turkmenistan",
  "Uganda",	
  "Uzbekistan",
  "Vatican City",
  "West Bank",
  "Zambia",	
  "Zimbabwe")


# Combining datasets
combined <- inner_join(trade_data, harbor_data, by = "country_code") %>% 
  inner_join(econ_data, by = c("country_code")) %>%
  inner_join(polity_data, by = c("country_code")) %>% 
  filter(!is.na(polity2)) %>% select(country_code, country, year, harbors, n_harbors,
                                rgdpe, pop, trade, c_area, polity2)  
  









#polity_data$country_code[!(polity_data$country_code %in% econ_data$country_code)]


