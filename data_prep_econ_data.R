
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


#######################################
## Generating the country level data ##
#######################################

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










#######################################
## Generating the country level data ##
#######################################

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

pop_density_2005 <- raster("data/population_density/gpw-v4-population-density-rev10_2005_30_sec_tif/gpw_v4_population_density_rev10_2005_30_sec.tif")
pop_density_projected_2005 <- projectRaster(pop_density_2005, crs = newcrs, method = "bilinear")

# Extracting the values
lights_data <- rep(NA, length(coast_hexagons@polygons))
pop_density_data_2000 <- rep(NA, length(coast_hexagons@polygons))
pop_density_data_2005 <- rep(NA, length(coast_hexagons@polygons))
pop_density_data_2010 <- rep(NA, length(coast_hexagons@polygons))
pop_density_data_2015 <- rep(NA, length(coast_hexagons@polygons))
pop_density_data_2020 <- rep(NA, length(coast_hexagons@polygons))

for (i in 1:length(coast_hexagons@polygons)){
  lights_data[i] <- mean(values(crop(lights_small_projected, 
                                     coast_hexagons[i])), na.rm=TRUE)
  
  pop_density_data_2000[i] <- mean(values(crop(pop_density_data_2000, 
                                          coast_hexagons[i])), na.rm=TRUE)
  pop_density_data_2005[i] <- mean(values(crop(pop_density_data_2005, 
                                          coast_hexagons[i])), na.rm=TRUE)
  pop_density_data_2010[i] <- mean(values(crop(pop_density_data_2010, 
                                          coast_hexagons[i])), na.rm=TRUE)
  pop_density_data_2015[i] <- mean(values(crop(pop_density_data_2015, 
                                          coast_hexagons[i])), na.rm=TRUE)
  pop_density_data_2020[i] <- mean(values(crop(pop_density_data_2020, 
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
remove <- c("Åland", "Albania", "Australia", "Canada", "Denmark", "Norway", 
            "Sweden", "Germany","France", "Greenland", "Iceland", "Italy", 
            "Spain", "Russia", "Greece","Japan", "New Zealand", "Ukraine", 
            "United Kingdom", "United States of America", "Finland", "Poland",
            "Netherlands", "Portugal", "Croatia", "Romania", "Ireland", 
            "Lithuania","Estonia", "Bulgaria", "Montenegro", "Belgium", 
            "Isle of Man", "Latvia", "Jersey", "Guernsey", "Cyprus", "N. Cyprus")

countries_list1 <- countries_list[!(countries_list@data$ADMIN%in%remove),]

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
































## Preliminary analysis
library(plm)
library(stargazer)
library(sandwich)
library(lmtest) 
library(AER)

## Clustering standard errors
robust_std <- function(group, model){
  G <- length(unique(group))
  N <- length(group)
  dfa <- (G/(G - 1)) * (N - 1)/model$df.residual
  coeftest(model, 
           vcov=function(x) dfa*vcovHC(x, cluster="group", 
                                       type="HC0"))[, "Std. Error"]
}

# Models: Population density
fs1 <- lm(data=coastal_data_fe, formula = as.numeric(y) ~ y_p)
se_fs1 <- robust_std(coastal_data_fe$country_var, fs1)

fs2 <- lm(data=coastal_data_fe, formula = as.numeric(y) ~ y_p +factor(country_var))
se_fs2 <- robust_std(coastal_data_fe$country_var, fs2)

m1 <- lm(data=coastal_data_fe, formula = log(1+lights_data) ~ y +factor(country_var))
se_m1 <- robust_std(coastal_data_fe$country_var, m1)

m2 <- lm(data=coastal_data_fe, formula = log(1+lights_data) ~ y_p+factor(country_var))
se_m2 <- robust_std(coastal_data_fe$country_var, m2)

iv1 <- ivreg(log(1+lights_data)~ y +factor(country_var) | y_p  +
                   factor(country_var), data=coastal_data_fe)
se_iv1 <- robust_std(coastal_data_fe$country_var, iv1)


# Adjust F statistic 
#wald_results <- waldtest(output, vcov = cov1)
star <- stargazer(fs1,fs2, m2, m1,iv1, type = "text",
          se        = list(se_fs1, se_fs2,se_m2, se_m1, se_iv1),
          omit.stat = c("rsq", "f", "ser"),
          header = FALSE,
          column.labels   = c("OLS", "IV"),
          column.separate = c(4, 1),
          dep.var.labels.include = TRUE,
          model.names = FALSE,
          star.char = c(""), 
          dep.var.labels=c("Ports","Pop. density"),
          style="io",
          font.size="small",
          digits = 2,
          covariate.labels = c("$\\widehat{Ports}$", "Ports"),
          omit=c("country_var","year","Constant"))
          

note.latex <- "\\multicolumn{6}{c} {\\parbox[c]{11cm}{\\textit{Notes:} Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long and interesting comment.}} \\\\"
star[grepl("Note",star)] <- note.latex
cat (star, sep = "\n")

          
# Models: Night lights
fs1 <- lm(data=coastal_data_fe, formula = as.numeric(y) ~ y_p)
se_fs1 <- robust_std(coastal_data_fe$country_var, fs1)

fs2 <- lm(data=coastal_data_fe, formula = as.numeric(y) ~ y_p +factor(country_var))
se_fs2 <- robust_std(coastal_data_fe$country_var, fs2)

m1 <- lm(data=coastal_data_fe, formula = lights_data ~ y +factor(country_var))
se_m1 <- robust_std(coastal_data_fe$country_var, m1)

m2 <- lm(data=coastal_data_fe, formula = lights_data ~ y_p+factor(country_var))
se_m2 <- robust_std(coastal_data_fe$country_var, m2)

iv1 <- ivreg(lights_data~ y +factor(country_var) | y_p  +
               factor(country_var), data=coastal_data_fe)
se_iv1 <- robust_std(coastal_data_fe$country_var, iv1)


# Adjust F statistic 
#wald_results <- waldtest(output, vcov = cov1)
star <- stargazer(fs1,fs2,m2, m1,iv1, type = "text",
                  se        = list(se_fs1, se_fs2,se_m2, se_m1, se_iv1),
                  omit.stat = c("rsq", "f", "ser"),
                  header = FALSE,
                  column.labels   = c("OLS", "IV"),
                  column.separate = c(4, 1),
                  dep.var.labels.include = TRUE,
                  model.names = FALSE,
                  star.char = c(""), 
                  dep.var.labels=c("Ports","Night lights"),
                  style="io",
                  font.size="small",
                  digits = 2,
                  covariate.labels = c("$\\widehat{Ports}$", "Ports"),
                  omit=c("country_var","year","Constant"))


note.latex <- "\\multicolumn{6}{c} {\\parbox[c]{11cm}{\\textit{Notes:} Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long and interesting comment.}} \\\\"
star[grepl("Note",star)] <- note.latex
cat (star, sep = "\n")



summary(plm(density_data~as.factor(y),index = c("country_var"), model = "within",  
            data = coastal_data_fe1))
iv_gdp3 <- ivreg(density_data~ y +factor(country_var) | y_pred  +factor(country_var), data=coastal_data_fe1)
summary(iv_gdp3)

summary(lm(as.numeric(y) ~ pred, data = coastal_data_fe1))


rse1 <- sqrt(diag(vcovHC(iv_gdp3, type = "HC1")))[2]

m1 <- lm(formula = y ~factor(country_var), data = coastal_data_fe1)
m2 <- lm(formula = y_pred ~  factor(country_var), data = coastal_data_fe1)

coastal_data_fe1$fvalues <- as.numeric(coastal_data_fe1$y)-predict(m1,coastal_data_fe1)
coastal_data_fe1$fvalues_pred <- coastal_data_fe1$y_pred-predict(m2,coastal_data_fe1)

ggplot(data=coastal_data_fe1, aes(x=(fvalues_pred), y=( fvalues)))+ 
  geom_rug(alpha = 0.01) + xlab("") + ylab("")+ggtitle("Nightlights and port suitability")+
  geom_smooth()+

  stat_summary_bin(fun.y='mean', bins=500,color='blue',alpha=0.5, size=2, geom='point')
    

# first stage table, cross section table, validation using night lights table
  
  
  







