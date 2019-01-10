if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rgeos, viridis, ranger)

## Setting theme
theme_set(theme_bw() + theme(panel.grid.minor = element_line(colour = "white", size = 0.5),
                             panel.grid.major = element_line(colour = "white", size = 0.2)))


###########################
## Joining the dataframe ##
###########################
#hexagons_full <- gIntersection(hexagons, study_area, byid = TRUE)


model <- ranger(formula= as.numeric(y)~., data=training_data, num.trees = 1000, mtry = 4)
prediction <- predict(model, dataset)$predictions

dataset_final <- dataset %>%  dplyr::select(y) %>% mutate(y_pred=prediction)
row.names(dataset_final) <- row.names(data)

sps_df <- SpatialPolygonsDataFrame(hexagons, dataset_final, match.ID = TRUE)


#######################
## Plotting the data ##
#######################

# Plotting a country
plot_country <- function(x){
  
  # Define the datasets
  study_area_country <- countries10[countries10$ADMIN==x,]
  dataset_country <- gIntersection(sps_df, study_area_country, byid = TRUE)
  ports_country <<- gIntersection(study_area_country, ports, byid = TRUE)
  
  # Set the same row names and match the datasets
  row.names(dataset_country) <- gsub("\\s.*", "", sapply(dataset_country@polygons, function(x) x@ID))
  dataset_country_final <<- sps_df[row.names(sps_df) %in% sapply(dataset_country@polygons, function(x) x@ID), ]
}

plot_country("Sweden")

# Plotting the predicted ports
tm_shape(dataset_country_final) +  tm_fill(col="y") 


dataset_country_final@data$pred <- ifelse(dataset_country_final@data$y_pred>=1.6, 1, 0)




  tm_shape(ports_country) + tm_dots(size=0.3)  palette=plasma(256)







countries_list <- countries10[countries10$TYPE=="Sovereign country",]

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


## Plotting the results: Reduced form relationship, shape=1

# Basic relationship
countries_list@data %>% filter(n_harbors>0) %>% 
  group_by(n_harbors) %>%
  summarize(gdp=mean(GDP_MD_EST), obs=n()) %>% 
  ggplot(aes(x=log(n_harbors), y=log(as.numeric(gdp)), size=obs)) + geom_point(alpha=0.3) +
  geom_smooth(method = "lm", se = TRUE, color="Black", size=0.5) + 
  xlab("log(Harbors)") + labs(caption = "") + ylab("log(GDP per capita)") +
  theme(legend.position="none")

# By continent
countries10@data %>% filter(n_harbors>0) %>% 
  group_by(n_harbors) %>%
  mutate(gdp=(GDP_MD_EST/as.numeric(POP_EST))) %>%
  #summarize(gdp=mean(GDP_MD_EST/as.numeric(POP_EST)), obs=n()) %>% 
  ggplot(aes(x=log(n_harbors), y=log(as.numeric(gdp)),color=CONTINENT)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="Black") + 
  xlab("log(Harbors)") + labs(color = "Continents") + ylab("log(GDP per capita)") 

# With labels
countries10@data %>% filter(n_harbors>0) %>% 
  group_by(n_harbors) %>%
  mutate(gdp=(GDP_MD_EST/as.numeric(POP_EST))) %>%
  filter(CONTINENT!="Antarctica", CONTINENT!="Seven seas (open ocean)") %>% 
  #summarize(gdp=mean(GDP_MD_EST/as.numeric(POP_EST)), obs=n()) %>% 
  ggplot(aes(x=log(n_harbors), y=log(as.numeric(gdp)))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="Black") + 
  xlab("log(Harbors)") + labs(caption = "") + ylab("log(GDP per capita)") +
  geom_text(aes(label=SOVEREIGNT),hjust=0, vjust=0)  +
  theme(legend.position="none")

# First stage
countries_list@data %>% filter(n_harbors>0) %>% 
  group_by(harbors) %>%
  summarize(n_harbors=mean(n_harbors), obs=n()) %>% 
  ggplot(aes(x=log(harbors), y=log(n_harbors))) + geom_point(alpha=0.2, size=4, color="Blue") +
  geom_smooth(method = "lm", se = FALSE, color="Black", size=0.5) + 
  xlab("Harbors") + labs(caption = "") + ylab("Predicted harbors") +
  theme(legend.position="none")

# To do: first stage, check the countries, set up the slides













## Averaging over raster files, fungerer
pop_density <- raster("data/population_density/gpw_v4_population_density_rev10_2010_2pt5_min.tif")

#study_area <- countries10[countries10$CONTINENT=="Sweden",]

## Averaging by country
#hexagons_samerica










#joining country polygons with the predictions, 

# Iceland
training_data <- training_data[sample(1:nrow(data)),]
model_testing <- ranger(formula= y~., data=data, num.trees = 300, mtry = 40)
prediction <- predict(model_testing, dataset_africa)$predictions
prediction <- ifelse(prediction>=0.6, 1, 0)

table(dataset_iceland$y, prediction)
dataset_africa$prediction <-  prediction


# den tror alt langs vannet er en port hvis man ikke trener den med innlandet.


#hexagons_full <- gIntersection(hexagons_iceland, study_area, byid = TRUE)

#ID <- sapply(hexagons_iceland@polygons, function(x) x@ID)
#eval_df <- data.frame(dataset_iceland$y, prediction)
#row.names(eval_df) <- ID
sps_df <- c()
sps_df <- SpatialPolygonsDataFrame(hexagons_africa, dataset_africa, match.ID = TRUE)
sps_df <- sps_df[sps_df$prediction==1,]

  tm_shape(study_area_namerica) +
    tm_borders() +
  tm_shape(sps_df) +
    tm_fill(col="prediction", palette = plasma(256)) + tm_layout(frame=FALSE) 

  
  
  

n_harbors <- c()
for (i in study_area_africa@data$ADMIN){
  nr <- sum(sapply(1:nrow(sps_df), function(x) gIntersects(sps_df[x,], study_area_africa[study_area_africa$ADMIN==i,])))
  n_harbors[which(study_area_africa@data$ADMIN==i)] <- nr
  print(c(i, nr))
}


study_area_africa@data$n_harbors <- n_harbors
  
  
  gdp_cap <- study_area_africa@data$GDP_MD_EST / as.numeric(study_area_africa@data$POP_EST)
  study_area_africa@data$gdp_cap <- gdp_cap
  study_area_africa@data$n_harbors <- study_area_africa@data$n_harbors / as.numeric(study_area_africa@data$POP_EST)


plot(log(study_area_africa@data$n_harbors), log(study_area_africa@data$gdp_cap))

ggplot(data=study_area_africa@data, aes(x=log(n_harbors), y=log(gdp_cap))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


final <- rbind(study_area_namerica, study_area_samerica, study_area_africa, study_area_europe)


ggplot(data=final@data, aes(x=log(n_harbors), y=log(gdp_cap))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)










# To do: automate, remove weird areas, normalize by population or area?








