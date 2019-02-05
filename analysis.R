
## Setting theme
theme_set(theme_bw() + theme(panel.grid.minor = element_line(colour = "white", size = 0.5),
                             panel.grid.major = element_line(colour = "white", size = 0.2)))

library(stargazer)
# Summary statistics
combined %>% select(-year) %>% 
  as.data.frame%>% stargazer(type="text",
                             covariate.labels=c("Ports","Harbors","Gdp","Population","Exports","Area", "Polity"),
                             digits=1, omit.summary.stat = c("p25","p75"))
                             
  
  
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

plot_country("China")

# Defining variables
dataset_country_final@data$pred <- ifelse(dataset_country_final@data$y_pred>=1.5, 1, 0)
dataset_country_final@data$false_p <- ifelse(as.numeric(dataset_country_final@data$y)-1<dataset_country_final@data$pred, 2, 0)
dataset_country_final@data$false_n <- ifelse(as.numeric(dataset_country_final@data$y)-1>dataset_country_final@data$pred, 1, 0)
dataset_country_final@data$errors <- dataset_country_final@data$false_p + dataset_country_final@data$false_n

# Plotting the predicted ports
p1 <- tm_shape(dataset_country_final) +  tm_fill(col="y_pred", palette=plasma(256)) + tm_layout(frame=FALSE, legend.show=TRUE)
p2 <- tm_shape(dataset_country_final) +  tm_fill(col="errors", palette=plasma(256), title = "Figure 1a") + tm_layout(frame=FALSE, legend.show=FALSE)
tmap_arrange(p1,p2)




## Plotting elevation and port data
elev_tmp <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/ETOPO1_Ice_g_geotiff.tif") 
countries10_tmp <- ne_download(scale = 10, type = 'countries', category = 'cultural')
spain <- countries10_tmp[countries10$ADMIN=="Sweden",]

# Projecting the shapefile
crs(elev_tmp) <- crs(countries10_tmp)

# Note: in general buffering should be done on projected data. However,
#       here it is fine since it just captures the whole area.

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

#+














## Plotting the results: Reduced form relationship

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

# Results

combined <- combined %>% mutate(n_harbors=log(1+n_harbors),
                                harbors=log(1+harbors),gdp=log(rgdpo/pop)) 




ggplotRegression(lm(gdp ~ n_harbors, data = combined)) + 
  xlab("log(Harbors)") + ylab("log(Ports)") + theme(plot.title = element_text(size = 10))

ggplotRegression(lm(harbors ~ n_harbors + area, data = dta_tmp)) + xlab("log(Harbors)") + ylab("log(Ports)")
ggplotRegression(lm(pop ~ n_harbors + area, data = dta_tmp)) + xlab("log(Harbors)") + ylab("log(Population)")
ggplotRegression(lm(gdp ~ n_harbors + area , data = dta_tmp)) + xlab("log(Harbors)") + ylab("log(Gdp)")


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





est.1 <- lm(harbors ~ n_harbors + area, data=dta_tmp)
summary(est.1)





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








library(plm)

summary(lm(log(trade) ~ log(harbors+1) + length + long + lat, data = combined))
summary(plm(log(trade) ~ log(n_harbors+1) + length + long + lat, data = combined, index = c("continent"), model = "within"))
summary(plm(log(rgdpe) ~ log(n_harbors+1) + length + long + lat, data = combined, index = c("continent"), model = "within"))
summary(lm(log(rgdpe) ~ (n_harbors) + c_area, data = combined))
summary(plm(polity2 ~ (n_harbors) + c_area, data = combined, index = c("continent"), model = "within"))


ggplot(data=combined, aes(y=log(1+harbors), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm",col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(trade), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(rgdpe), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(pop), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)



ggplot(data=combined, aes(y=log(trade), x = log(1+n_harbors)))+ geom_point(alpha=0.5)+stat_smooth(method = "lm", col = "black", se=TRUE)
ggplot(data=combined, aes(y=log(rgdpe), x = log(1+n_harbors)))+ geom_point()
ggplot(data=combined, aes(y=(polity2), x = log(1+n_harbors)))+ geom_point()+
  geom_text(aes(label=country_code),hjust=0, vjust=0)







library(AER)

rf_gdp1 <- lm(pop ~ n_harbors , data=combined)
rf_gdp2<- lm(pop ~ n_harbors + area, data=combined)

iv_gdp1 <- ivreg(pop ~ harbors | n_harbors , data=dta_tmp)
iv_gdp2<- ivreg(pop ~ harbors + area| n_harbors + area, data=dta_tmp)

stargazer(rf_gdp1, rf_gdp2, type="latex", 
          star.char = c(""), dep.var.caption = "",
          dep.var.labels.include = FALSE, column.labels   = c("OLS", "IV"),
          column.separate = c(2, 2),notes = "The dependent variable is the logarithm of population.", notes.append = FALSE,
          model.names = FALSE, covariate.labels = c("Harbors", "Area", "Ports"), header=FALSE,font.size="tiny", omit.stat = c("rsq", "f"))
#,single.row = TRUE,column.sep.width = "1pt"








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






# Urban population
pop <- read_excel("data/mpd2018 (1).xlsx", sheet="pop", skip=1) %>% 
  filter(year==1500|year==2000) %>% 
  select_if(~!any(is.na(.))) %>% 
  gather("country", "pop", 2:49) %>%
  rename("country_code"="country") 


combined_pop <- inner_join(pop, harbor_data, by = "country_code") 
  
summary(lm(data=combined_pop, formula=log(pop)~log(1+n_harbors)*factor(year)+factor(country)))




y = a_i + post + treat + treat*post + e


