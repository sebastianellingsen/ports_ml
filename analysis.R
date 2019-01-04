if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rgeos, viridis, ranger)


###########################
## Joining the dataframe ##
###########################
#hexagons_full <- gIntersection(hexagons, study_area, byid = TRUE)


model <- ranger(formula= as.numeric(y)~., data=dataset, num.trees = 500, mtry = 4)
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

# Plotting the predicted ports
tm_shape(dataset_country_final) +  tm_fill(col="y_pred", palette=plasma(256)) 
+
  tm_shape(ports_country) + tm_dots(size=0.3) 





n_harbors <- c()
# use sapply here:
for (i in study_area@data$ADMIN){
  study_area_country <- countries10[countries10$ADMIN==i,]
  dataset_country <- gIntersection(sps_df, study_area_country, byid = TRUE)
  row.names(dataset_country) <- gsub("\\s.*", "", sapply(dataset_country@polygons, function(x) x@ID))
  sps_df_temp <- sps_df[row.names(sps_df) %in% sapply(dataset_country@polygons, function(x) x@ID), ]
  
  n_harbors[which(study_area@data$ADMIN==i)] <- sum(ifelse(sps_df_temp@data$y_pred>1.5,1,0))
  
  print(i)
}

countries10@data$n_harbors <- n_harbors


# Plotting the results
ggplot(data=countries10@data, aes(x=log(n_harbors), y=log(GDP_MD_EST))) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


countries10@data %>% filter(n_harbors>0) %>% ggplot(aes(x=log(n_harbors), y=log(as.numeric(GDP_MD_EST)))) + geom_point() +
  geom_smooth(method = "lm", se = TRUE)


# add actual harbors as well.






















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


#gIntersects(sps_df[1,], k)
sum(sapply(1:nrow(sps_df), function(x) gIntersects(sps_df[x,], k)))



#Lop pver hvert land, tell hvor mange som overlapper der. Skal vaere raskere enn den andre metoden.




k <- study_area_samerica[study_area_samerica$ADMIN=="Bolivia",]







