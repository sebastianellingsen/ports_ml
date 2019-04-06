## This script performs the analysis of the tanzam railway expansion
if (!require(pacman)) install.packages("pacman")
p_load(tidyverse, sf, raster, tmap, sp, rgdal, rnaturalearth, rgeos, 
       lmtest, sandwich, broom, spatialEco)

newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")


##################################################
########### Preparing and loading data ###########
##################################################


## Country data ##
countries10 <- ne_download(scale = 10, 
                           type = 'countries', category = 'cultural')

africa <- countries10[countries10@data$CONTINENT=="Africa",]
africa <- spTransform(africa, newcrs)

tan_zam=africa[africa@data$SOVEREIGNT=="Malawi"|
                 africa@data$SOVEREIGNT=="Zambia"|
                 africa@data$SOVEREIGNT=="United Republic of Tanzania",]
zambia=africa[africa@data$SOVEREIGNT=="Zambia",]


## Railway data ##
railway_zam <- readOGR("data/ZMB_rrd/ZMB_rails.shp", "ZMB_rails")
railway_zam <- spTransform(railway_zam, newcrs)

railway_tan <- readOGR("data/TZA_rrd/TZA_rails.shp", "TZA_rails")
railway_tan <- spTransform(railway_tan, newcrs)

tanzam_tan <- railway_tan[railway_tan@data$FID_rail_d==174706|
                            railway_tan@data$FID_rail_d==174597,]
tanzam_zam <- railway_zam[railway_zam@data$FID_rail_d==174985|
                            railway_zam@data$FID_rail_d==175916,]

# Tanzam railway
rail <- c(175790, 175899, 175917, 175925, 176219, 176780, 175900, 175909, 
          175915, 175925,176219, 176220, 176237, 176240, 176242, 176243, 
          176246)

railway_zam_pre <- railway_zam[railway_zam@data$FID_rail_d %in% rail,]

## City data ##
africa_polis <- readOGR("/Users/sebastianellingsen/Dropbox/ports_ml/data/population_density/Africapolis_2015_shp/Africapolis.shp", "Africapolis")

africa_polis@data$pop1960<-(as.numeric(as.character(africa_polis@data$pop1960)))
africa_polis@data$pop1970<-(as.numeric(as.character(africa_polis@data$pop1970)))
africa_polis@data$pop2000<-(as.numeric(as.character(africa_polis@data$pop2000)))
africa_polis_ssa <- africa_polis[!(africa_polis@data$ISO %in% c("MAR", "EGY",
                                                                "TUN", "LBY",
                                                                "DZA")),]

africa_polis_ssa_1 <- remove.holes(africa_polis_ssa)
africa_polis_ssa_plot <- SpatialPolygonsDataFrame(africa_polis_ssa_1,
                                                  africa_polis_ssa@data, 
                                                  match.ID = TRUE)



##################################################
########### Preparing dataset ####################
##################################################
africa_polis <- readOGR("data/population_density/Africapolis_2015_shp/Africapolis.shp", 
                        "Africapolis")
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")
africa_polis <- spTransform(africa_polis, newcrs)

distance_rail <- c()
counter <- 0

for (i in 1:nrow(africa_polis@data)){
  id <- africa_polis@data[i,1]
  cell <- africa_polis[africa_polis@data$ID==id,]
  distance_rail[i] <- gDistance(railway_zam_pre, cell)
  
  counter <- counter+1
  print(counter/length(row.names(africa_polis@data)))
}

africa_polis@data$distance_rail <- distance_rail
rail_cities <- africa_polis[africa_polis@data$ISO=="ZMB"|
                              africa_polis@data$ISO=="TZA",]

rail_cities@data$pop2010 <- as.numeric(as.character(rail_cities@data$pop2010))
rail_cities@data$pop1950 <- as.numeric(as.character(rail_cities@data$pop1950))


tanzania=africa[africa@data$SOVEREIGNT=="United Republic of Tanzania",]
rail_start <-rail_cities[rail_cities@data$ID==1325,]
copperbelt <-rail_cities[rail_cities@data$ID==136,]

distance_tanzania <- c()
distance_start <- c()
distance_tanzam <- c()
distance_copperbelt <- c()
counter <- c()

for (i in 1:nrow(rail_cities@data)){
  id <- rail_cities@data[i,1]
  cell <- rail_cities[rail_cities@data$ID==id,]
  distance_start[i] <- gDistance(rail_start, cell)
  distance_tanzania[i] <- gDistance(tanzania, cell)
  distance_tanzam[i] <- gDistance(tanzam_zam, cell)
  distance_copperbelt[i] <- gDistance(copperbelt, cell)
  
  counter <- counter+1
  print(counter/length(row.names(rail_cities@data)))
}

rail_cities@data$distance_start <- distance_start
rail_cities@data$distance_tanzania <- distance_tanzania
rail_cities@data$distance_tanzam <- distance_tanzam
rail_cities@data$distance_copperbelt <- distance_copperbelt

rail_cities_data <- rail_cities@data %>% 
  rename("1950"=pop1950, "1960"=pop1960, "1970"=pop1970, "1980"=pop1980,
         "1990"=pop1990, "2000"=pop2000, "2010"=pop2010) %>% 
  gather("year", "pop",3:9) %>% 
  mutate(treat=ifelse(year>=1980,1,0),
         rail=ifelse(distance_rail==0,1,0)) %>% 
  mutate(pop=as.numeric(pop), city=ifelse(pop>0,1,0)) %>% 
  filter(year>=1950 & year<=2010) %>% 
  filter(ISO=="ZMB")%>% 
  filter(distance_tanzania>498.8921,
         !(Name %in% c("Serenje", "Mkushi", "Chembe", "Petauke", "Nyimba"))) %>% 
  mutate(lpop=log(pop+1), ldistance_start=log(distance_start+1)) #%>% 
#group_by(year) %>% 
#summarise(pop=sum(pop), city=sum(city))

#ggplot(data=rail_cities_data, aes(x=as.numeric(year),y=pop, group=close))+
#geom_line(shape=close)
#ggplot(data=rail_cities_data, aes(x=as.numeric(year),y=city))+
#  geom_line()

m1 <- lm(lpop~log(distance_start+1)*rail*factor(year), 
         data=rail_cities_data)
summary(m1)

m <- tidy(coeftest(m1, vcov = vcovHC(m1, type="HC1")))
m_terms <- m[c(23:28),]

year <- c("1960","1970","1980","1990","2000","2010")
m_terms$year <- as.numeric(year)

ggplot(data=m_terms, mapping=aes(x=year, y=estimate, group=1)) +
  geom_errorbar(aes(x = year, ymin = estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
  xlab("") + ylab("") + geom_point(size = 1, shape = 21, fill = "black") +
  geom_vline(xintercept=1975, color="black", linetype="dashed", size=0.3, alpha = 0.5)+ 
  geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5)+ 
  theme(strip.background = element_blank(), strip.placement = "outside")+
  ggtitle("City size and distance to the TAZARA link")+
  theme(plot.title = element_text(size = 7))




## This loop can be used to find the correct section of the shapefile 
# plot(zambia)
# plot(tanzam_zam,add=TRUE)
# for (i in 1:nrow(tanzam_zam@data)){
#   rail_nr <- tanzam_zam@data[i,]$FID_rail_d
#   railway_zam_1 <- railway_zam[railway_zam@data$FID_rail_d==rail_nr,]
#   plot(railway_zam_1,add=TRUE)
#   print(rail_nr)
#   print(i)
#   Sys.sleep(1)
# }

## This section plots the data in the analysis
# rail_cities_1 <- rail_cities[rail_cities@data$distance_tanzania>450,]
# rail_cities_1 <- rail_cities_1[!(rail_cities_1@data$Name %in% c("Serenje", "Mkushi", "Chembe", "Petauke", "Nyimba")),]
# 
# tm_shape(zambia) +
#   tm_borders(col = "grey", lwd=0.5) +
#   tm_shape(railway_zam_pre)+tm_lines("red") +
#   tm_shape(tanzam_zam)+tm_lines("blue") +
#   #tm_shape(africa_polis_ssa_plot) + tm_dots(size=0.2)+
#   tm_layout(frame=FALSE,legend.title.size=0.95) +
#   tm_shape(rail_cities_1) +
#   tm_dots(size=0.1, shape=1) +
#   tm_shape(rail_start)+
#   tm_dots()+
#   tm_layout(frame=FALSE,legend.title.size=0.95)

# tm_shape(tan_zam) +
#   tm_borders(col = "grey", lwd=0.5) +
#   tm_shape(tanzam_zam)+tm_lines()+
#   tm_shape(tanzam_tan)+tm_lines()+
#   tm_layout(frame=FALSE,legend.title.size=0.95)+
#   tm_shape(africa_polis_ssa_plot)+
#   tm_bubbles(size="pop1960",scale=4,border.lwd = 0.01,
#              style = "pretty", alpha=0.65)

# plot(zambia)
# plot(railway_zam_pre,add=TRUE)
# 
# tm_shape(zambia) +
#   tm_borders(col = "grey", lwd=0.5) +
#   tm_shape(railway_zam_pre)+tm_lines("red") +
#   tm_shape(tanzam_zam)+tm_lines("blue") +
#   #tm_shape(africa_polis_ssa_plot) + tm_dots(size=0.2)+
#   tm_layout(frame=FALSE,legend.title.size=0.95) +
#   tm_shape(africa_polis_ssa_plot) +
#   tm_bubbles(size="pop2000",scale=4,border.lwd = 0.01,
#              style = "pretty", alpha=0.65) +
#   tm_shape(rail_start)+
#   tm_dots()+
#   tm_layout(frame=FALSE,legend.title.size=0.95)





