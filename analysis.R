
#####################################################################
## This file produces the regressions and the plots used in the paper
####################################################################

###############################
## Loading data and packages ##
###############################

## Setting theme
theme_set(theme_bw() + theme(panel.grid.minor = element_line(colour = "white", size = 0.5),
                             panel.grid.major = element_line(colour = "white", size = 0.2)))

library(stargazer)
# Summary statistics
combined %>% select(-year) %>% 
  as.data.frame%>% stargazer(type="text",
                             covariate.labels=c("Ports","Harbors","Gdp","Population","Exports","Area", "Polity"),
                             digits=1, omit.summary.stat = c("p25","p75"))
                             
  
  
######################################################
## Plotting the fitted values for various countries ##
######################################################

plot_country <- function(x){
  
  # Define the datasets
  study_area_country <- countries10[countries10$ADMIN==x,]
  dataset_country <- gIntersection(sps_df, study_area_country, byid = TRUE)
  ports_country <<- gIntersection(study_area_country, ports, byid = TRUE)
  
  # Set the same row names and match the datasets
  row.names(dataset_country) <- gsub("\\s.*", "", sapply(dataset_country@polygons, function(x) x@ID))
  dataset_country_final <<- sps_df[row.names(sps_df) %in% sapply(dataset_country@polygons, function(x) x@ID), ]
}

plot_country("China")

# Defining variables
dataset_country_final@data$pred <- ifelse(dataset_country_final@data$y_pred>=1.6, 1, 0)
dataset_country_final@data$false_p <- ifelse(as.numeric(dataset_country_final@data$y)-1<dataset_country_final@data$pred, 2, 0)
dataset_country_final@data$false_n <- ifelse(as.numeric(dataset_country_final@data$y)-1>dataset_country_final@data$pred, 1, 0)
dataset_country_final@data$errors <- dataset_country_final@data$false_p + dataset_country_final@data$false_n

# Plotting the predicted ports
p1 <- tm_shape(dataset_country_final) +  tm_fill(col="y_pred", palette=plasma(256)) + tm_layout(frame=TRUE, legend.show=FALSE,bg.color="grey85") 
p2 <- tm_shape(dataset_country_final) +  tm_fill(col="errors", palette=plasma(256), title = "Figure 1a") + tm_layout(frame=TRUE, legend.show=FALSE,bg.color="grey85")
tmap_arrange(p1,p2)






######################################
## Plotting elevation and port data ##
######################################
# Note: buffering done on unprojected data 

elev_tmp <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/ETOPO1_Ice_g_geotiff.tif") 
countries10_tmp <- ne_download(scale = 10, type = 'countries', category = 'cultural')
spain <- countries10_tmp[countries10$ADMIN=="Sweden",]

# Projecting the shapefile
crs(elev_tmp) <- crs(countries10_tmp)

# Buffer around region
spain_buffer <- gBuffer(spain, width = 1)
elev_cropped = crop(elev_tmp, spain_buffer)
elev_masked = raster::mask(elev_cropped, spain_buffer)
elev_masked[elev_masked < 0] <- 0

# Set the same projection (units are in km)
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
spain <- spTransform(spain, newcrs)
spain_buffer <- spTransform(spain_buffer, newcrs)
elev <- projectRaster(elev_masked, crs = newcrs)

# Generating the hexagons
size <- 30
hex_points_tmp <- spsample(spain_buffer, type = "hexagonal", cellsize = size)
hex_grid_tmp <- HexPoints2SpatialPolygons(hex_points_tmp, dx = size)
hexagons_tmp <- gIntersection(hex_grid_tmp, spain, byid = TRUE)
elev_simple <- aggregate(elev, fact=4)

# The raster can be simplified to speed up the calculations
elevations <- sapply(1:length(hexagons_tmp@polygons), function(x) mean(values(mask(elev_simple, hexagons_tmp[x])), na.rm=TRUE))
ID <- sapply(hexagons_tmp@polygons, function(x) x@ID)
data_tmp <- data.frame(log(elevations))
row.names(data_tmp) <- ID
sps_df_tmp<- SpatialPolygonsDataFrame(hexagons_tmp, data_tmp, match.ID = TRUE)

library(tmap)

# Aggregating over the hexagons
tm_shape(sps_df_tmp) +
  tm_fill(col="log.elevations.", palette=plasma(256),n=20, labels = NULL) + 
  tm_layout(legend.show=FALSE, frame=FALSE)






#######################################
## Plotting elevation and bathymetry ##
#######################################
# Note: buffering done on unprojected data 

elev_tmp <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/ETOPO1_Ice_g_geotiff.tif") 
countries10_tmp <- ne_download(scale = 10, type = 'countries', category = 'cultural')
spain <- countries10_tmp[countries10$ADMIN=="China",]

# Projecting the shapefile
crs(elev_tmp) <- crs(countries10_tmp)

# Buffer around region
spain_buffer <- gBuffer(spain, width = 2)
elev_cropped = crop(elev_tmp, spain_buffer)
elev_masked = raster::mask(elev_cropped, spain_buffer)
#elev_masked[elev_masked < 0] <- 0

# Set the same projection (units are in km)
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
spain <- spTransform(spain, newcrs)
spain_buffer <- spTransform(spain_buffer, newcrs)
elev <- projectRaster(elev_masked, crs = newcrs)

# Generating the hexagons
size <- 60
hex_points_tmp <- spsample(spain_buffer, type = "hexagonal", cellsize = size)
hex_grid_tmp <- HexPoints2SpatialPolygons(hex_points_tmp, dx = size)
hexagons_tmp <- gIntersection(hex_grid_tmp, spain, byid = TRUE)
elev_simple <- aggregate(elev, fact=4)

# The raster can be simplified to speed up the calculations
elevations <- sapply(1:length(hexagons_tmp@polygons), function(x) mean(values(mask(elev_simple, hexagons_tmp[x])), na.rm=TRUE))
ID <- sapply(hexagons_tmp@polygons, function(x) x@ID)
data_tmp <- data.frame(log(elevations))
row.names(data_tmp) <- ID
sps_df_tmp<- SpatialPolygonsDataFrame(hexagons_tmp, data_tmp, match.ID = TRUE)

library(tmap)

# Aggregating over the hexagons
tm_shape(sps_df_tmp) +
  tm_fill(col="log.elevations.", palette=plasma(256),n=20, labels = NULL) + 
  tm_layout(legend.show=FALSE, frame=FALSE)

# Aggregating over the hexagons
tm_shape(elev_cropped) +
  tm_raster(midpoint = NA, palette=plasma(3), style="kmeans") + 
  tm_shape(spain) + tm_borders(col="white", lwd=0.7)+
  tm_layout(legend.show=FALSE, frame=TRUE)
  #tm_shape(hexagons_tmp)+tm_borders(col="white", lwd=0.3)




#########################################
## Plotting reduced form relationships ##
#########################################

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(alpha=0.3, color="Blue", size=3) +
    stat_smooth(method = "lm", col = "black", se=TRUE) +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " Se. =",signif(summary(fit)$coef[2,2], 5)))
}

combined <- combined %>% mutate(n_harbors=log(1+n_harbors),
                                harbors=log(1+harbors),gdp=log(rgdpo/pop)) 


ggplotRegression(lm(harbors ~ n_harbors, data = combined)) + 
  xlab("log(Harbors)") + ylab("log(Ports)") + theme(plot.title = element_text(size = 10))

ggplot(data=combined, aes(y=harbors, x=n_harbors))+geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)

ggplotRegression(lm(harbors ~ n_harbors + area, data = dta_tmp)) + xlab("log(Harbors)") + ylab("log(Ports)")
ggplotRegression(lm(pop ~ n_harbors + area, data = dta_tmp)) + xlab("log(Harbors)") + ylab("log(Population)")
ggplotRegression(lm(gdp ~ n_harbors + area , data = dta_tmp)) + xlab("log(Harbors)") + ylab("log(Gdp)")



#########################
## Running regressions ##
#########################

library(lmtest)

# Output as a dataframe
lmodel1_corrected_df <- lmodel1 %>% coeftest(vcoc=hccm) %>% tidy %>% data.frame

# add this to the function

library(broom)
library(stargazer)

est.iv  %>% coeftest(vcoc=hccm) %>% tidy %>% data.frame

library(AER)

combined <- combined %>% mutate(n_harbors=log(1+n_harbors),
                                harbors=log(1+harbors),gdp=log(rgdpo/pop)) 

rf_gdp1 <- lm(gdp ~ n_harbors , data=combined)
rf_gdp2<- lm(gdp ~ n_harbors + length  + c_area, data=combined)
rf_gdp3<- lm(gdp ~ n_harbors + length  + long+lat+ c_area + factor(continent), data=combined)
iv_gdp1 <- ivreg(gdp ~ harbors | n_harbors , data=combined)
iv_gdp2<- ivreg(gdp ~ harbors + length + long + lat + c_area | n_harbors + length + long + lat + c_area, data=combined)
iv_gdp3 <- ivreg(gdp ~ harbors  + length + long + lat + c_area  + factor(continent)| n_harbors  + length + long + lat + c_area  + factor(continent), data=combined)

star <- stargazer(rf_gdp1, rf_gdp2, rf_gdp3, iv_gdp1, iv_gdp2, iv_gdp3, type="text",style="io", 
          title="log(GDP)", star.char = c(""), dep.var.caption = "",
          dep.var.labels.include = FALSE, column.labels   = c("OLS", "IV"),
          column.separate = c(3, 3),notes = "I make this look good!", notes.append = FALSE,
          model.names = FALSE, omit = c("continent", "long", "lat", "length", "c_area", "Constant"), 
          covariate.labels = c("$\\widehat{Ports}$", "Ports"),font.size="small", 
          omit.stat = c("rsq", "f", "ser"), 
          add.lines = list(c("Controls","","$\\checkmark$","$\\checkmark$","","$\\checkmark$","$\\checkmark$"),c("Continent FE","","","$\\checkmark$","","","$\\checkmark$")))

note.latex <- "\\multicolumn{7}{c} {\\parbox[c]{11cm}{\\textit{Notes:} Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long and interesting comment.}} \\\\"
star[grepl("Note",star)] <- note.latex
cat (star, sep = "\n")

## Panel data section
library(plm)
library(broom)
lm_fixed <- plm(cgdppc ~ n_harbors*factor(year), data = combined, index = c("country.x", "year"), model = "within")
#summary(lm_fixed)

a <- tidy(lm_fixed, conf.int = TRUE)

a <- a[which(a$term=="n_harbors:factor(year)1960"):which(a$term=="n_harbors:factor(year)2013"),]

ggplot(a, aes(term,estimate)) +
  geom_point() +
  geom_vline(xintercept=which(a$term=="n_harbors:factor(year)1960"))

lm_fixed <- plm(gdp ~ p_high*factor(year), data = combined, index = c("country"), model = "within")
summary(lm_fixed)

a <- tidy(lm_fixed, conf.int = TRUE)

a <- a[which(a$term=="p_high:factor(year)1951"):which(a$term=="p_high:factor(year)2015"),]

ggplot(a, aes(term,estimate)) +
  geom_point() +
  geom_vline(xintercept=which(a$term=="n_harbors:factor(year)1960"))



## Urban population over time
pop <- read_excel("data/mpd2018 (1).xlsx", sheet="pop", skip=1) %>% 
  filter(year==1900|year==2000) %>% 
  select_if(~!any(is.na(.))) %>% 
  gather("country", "pop", 2:66) %>%
  rename("country_code"="country") 

combined_pop <- inner_join(pop, harbor_data, by = "country_code") 

combined_pop_1850 <- combined_pop %>% filter(year==1850) 
combined_pop_2000 <- combined_pop %>% filter(year==2000) 

summary(lm(data=combined_pop_1850, formula=log(pop)~n_harbors + c_area))
summary(lm(data=combined_pop_2000, formula=log(pop)~n_harbors + c_area))






######################
## Nighlights data ###
######################


lights <- raster("data/nightlights/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")
lights_small <- aggregate(lights, 4)

countries10_tmp <- ne_download(scale = 10, type = 'countries', category = 'cultural')
spain <- countries10_tmp[countries10$ADMIN=="Indonesia",]


crs(elev_tmp) <- crs(countries10_tmp)

# Buffer around region
spain_buffer <- gBuffer(spain, width = 2)
elev_cropped = crop(lights_small, spain_buffer)

elev_masked = raster::mask(elev_cropped, spain_buffer)

# Aggregating over the hexagons
tm_shape(elev_cropped) +
  tm_raster(midpoint = NA, palette=plasma(3),n=20) + 
  tm_shape(spain) + tm_borders(col="white", lwd=0.7)+
  tm_layout(legend.show=FALSE, frame=TRUE)
  #tm_shape(hexagons_tmp)+tm_borders(col="white", lwd=0.3)





lights <- raster("data/nightlights/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")
lights_small <- aggregate(lights, 4)

elev_tmp <- lights_small
countries10_tmp <- ne_download(scale = 10, type = 'countries', category = 'cultural')
spain <- countries10_tmp[countries10$ADMIN=="Indonesia",]

# Projecting the shapefile
crs(elev_tmp) <- crs(countries10_tmp)

# Buffer around region
spain_buffer <- gBuffer(spain, width = 2)
elev_cropped = crop(elev_tmp, spain_buffer)
elev_masked = raster::mask(elev_cropped, spain_buffer)
#elev_masked[elev_masked < 0] <- 0

# Set the same projection (units are in km)
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
spain <- spTransform(spain, newcrs)
spain_buffer <- spTransform(spain_buffer, newcrs)
elev <- projectRaster(elev_masked, crs = newcrs)

# Generating the hexagons
size <- 30
hex_points_tmp <- spsample(spain_buffer, type = "hexagonal", cellsize = size)
hex_grid_tmp <- HexPoints2SpatialPolygons(hex_points_tmp, dx = size)
hexagons_tmp <- gIntersection(hex_grid_tmp, spain, byid = TRUE)
elev_simple <- aggregate(elev, fact=4)

# The raster can be simplified to speed up the calculations
elevations <- sapply(1:length(hexagons_tmp@polygons), function(x) mean(values(mask(elev_simple, hexagons_tmp[x])), na.rm=TRUE))
ID <- sapply(hexagons_tmp@polygons, function(x) x@ID)
data_tmp <- data.frame((elevations))
row.names(data_tmp) <- ID
sps_df_tmp<- SpatialPolygonsDataFrame(hexagons_tmp, data_tmp, match.ID = TRUE) 

colnames(sps_df_tmp@data) <- "Night lights"

library(tmap)

# Aggregating over the hexagons
tm_shape(sps_df_tmp) +
  tm_fill(col="Night lights", palette=plasma(256), style="kmeans", labels = NULL) + 
  tm_layout(legend.show=TRUE, frame=TRUE, bg.color="grey85")









grid_cell <- sps_df@data
plot(grid_cell$y_pred, grid_cell$y)

grid_cell$y_pred <- as.numeric(grid_cell$y_pred)
grid_cell$y <- as.numeric(grid_cell$y)


library("AER")
binscatter(formula="y ~ y_pred",
           data=grid_cell, bins=10, partial=FALSE)


ggplot(grid_cell, aes(x = y_pred, y = y)) + 
  geom_point(alpha = .1)+stat_smooth(method = "lm", col = "black", se=TRUE)













## What features are predicted?
elev[elev>0] <- 0

sps_df_coastal <- SpatialPolygonsDataFrame(coast_hexagons, dataset_final, match.ID = TRUE)

lights_data_mean <- rep(NA, length(coast_hexagons@polygons))
lights_data_sd <- rep(NA, length(coast_hexagons@polygons))
for (i in 1:length(coast_hexagons@polygons)){
  
  hexagon_cropped <-  values(crop(elev, coast_hexagons[i]))
  
  #Points above water
  hexagon_above<- hexagon_cropped[hexagon_cropped>0]
  
  #Points above water
  hexagon_below <- hexagon_cropped[hexagon_cropped<=0]
  
  # Moments above
  lights_data_mean[i] <- raster::extract(elev, hexagon_above, fun=mean, na.rm=TRUE)
  lights_data_sd[i] <- raster::extract(elev, hexagon_above, fun=sd, na.rm=TRUE)
  
  # Moments below
  lights_data_mean[i] <- raster::extract(elev, hexagon_below, fun=mean, na.rm=TRUE)
  lights_data_sd[i] <- raster::extract(elev, coast_hexagons[i], fun=sd, na.rm=TRUE)
  
  print(i)
}





sps_df_coastal$elev_mean <-lights_data_mean
sps_df_coastal$elev_sd <-lights_data_sd


lights_reg_data <-  sps_df_coastal@data

lights_reg_data_1 <- lights_reg_data %>% 
  mutate(elev_data=log(1+elev_data_mean), elev_sd=log(1+elev_sd)) 

ggplot(data=lights_reg_data_1, aes(x=y_pred, y=elev_sd))+ geom_point(color='blue',alpha=0.05)+geom_rug(alpha = 0.01)

  
lights_data_mean <- raster::extract(elev, coast_hexagons[1], fun=mean, na.rm=TRUE)

hexagon_cropped <-  values(crop(elev, coast_hexagons[1]))
hexagon_cropped <- hexagon_cropped[hexagon_cropped>0]


