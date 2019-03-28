
#####################################################################
## This file prepares data on roads for the analysis of mechanisms ##
#####################################################################

## Setting the CRS
newcrs <- CRS("+proj=moll +datum=WGS84 +units=km")

## Loading data 
# Madagascar
madagascar_roads <- readOGR("data/roads/madagascar_roads/Madagascar_Roads.shp", "Madagascar_Roads")
madagascar_roads <- spTransform(madagascar_roads, newcrs)
madagascar_roads_paved <- madagascar_roads[madagascar_roads@data$SURFTYPE=="Paved",]

# Benin
benin_roads <- readOGR("data/roads/benin_roads/Benin_Roads.shp", "Benin_Roads")
benin_roads <- spTransform(benin_roads, newcrs)
benin_roads_paved <- benin_roads[benin_roads@data$SURF_TYPE_=="Asphalt Road",]

# Burkina Faso
burkina_faso_roads <- readOGR("data/roads/burkina-faso_roads/Burkina Faso_Roads.shp", 
                              "Burkina Faso_Roads")
burkina_faso_roads <- spTransform(burkina_faso_roads, newcrs)
burkina_faso_roads_paved <- burkina_faso_roads[burkina_faso_roads@data$SURF_TYPE1=="Asphalt",]

# Cameroon
cameroon_roads <- readOGR("data/roads/cameroon_roads/Cameroon_Roads.shp", 
                          "Cameroon_Roads")
cameroon_roads <- spTransform(cameroon_roads, newcrs)
cameroon_roads_paved <- cameroon_roads[cameroon_roads@data$SURF_TYPE1=="Asphalt",]

# Guinea
guinea_roads <- readOGR("data/roads/guinea_roads/Guinea_Roads.shp", 
                        "Guinea_Roads")
guinea_roads <- spTransform(guinea_roads, newcrs)
guinea_roads_paved <- guinea_roads[guinea_roads@data$SURF_TYPE1=="Asphalt",]

# Lesotho
lesotho_roads <- readOGR("data/roads/lesotho_roads/Lesotho_Roads.shp", 
                         "Lesotho_Roads")
lesotho_roads <- spTransform(lesotho_roads, newcrs)
lesotho_roads_paved <- lesotho_roads[lesotho_roads@data$SURFTYPE=="Paved",]

# Burundi
burundi_roads <- readOGR("data/roads/burundi_roads/Burundi_Roads.shp", 
                         "Burundi_Roads")
burundi_roads <- spTransform(burundi_roads, newcrs)
burundi_roads_paved <- burundi_roads[burundi_roads@data$SURF_TYPE1=="Asphalt",]

# Malawi
malawi_roads <- readOGR("data/roads/malawi_roads/Malawi_Roads.shp", 
                        "Malawi_Roads")
malawi_roads <- spTransform(malawi_roads, newcrs)
malawi_roads_paved <- malawi_roads[malawi_roads@data$SURFTYPE=="Paved",]

# Mali
mali_roads <- readOGR("data/roads/mali_roads/Mali_Roads.shp", 
                      "Mali_Roads")
mali_roads <- spTransform(mali_roads, newcrs)
mali_roads_paved <- mali_roads[mali_roads@data$SURF_TYPE1=="Asphalt",]

# Mauritania
mauritania_roads <- readOGR("data/roads/mauritania_roads/Mauritania_Roads.shp", 
                            "Mauritania_Roads")
mauritania_roads <- spTransform(mauritania_roads, newcrs)
mauritania_roads_paved <- mauritania_roads[mauritania_roads@data$SURF_TYPE1=="Asphalt",]

# Angola
angola_roads <- readOGR("data/roads/angola_roads/Angola_Roads.shp", 
                        "Angola_Roads")
angola_roads <- spTransform(angola_roads, newcrs)
angola_roads_paved <- angola_roads[angola_roads@data$SURFTYPE=="Paved",]

# Botswana
botswana_roads <- readOGR("data/roads/botswana_roads/Botswana_Roads.shp", 
                          "Botswana_Roads")
botswana_roads <- spTransform(botswana_roads, newcrs)
botswana_roads_paved <- botswana_roads[botswana_roads@data$SURFTYPE=="Paved",]

# Cote Divoire
cotedivoire_roads <- readOGR("data/roads/cote-divoire_roads/Cote dIvoire_Roads.shp", 
                             "Cote dIvoire_Roads")
cotedivoire_roads <- spTransform(cotedivoire_roads, newcrs)
cotedivoire_roads_paved <- cotedivoire_roads[cotedivoire_roads@data$SURF_TYPE1=="Asphalt",]

# Democratic Republic of Congo
drc_roads <- readOGR("data/roads/Democratic Re of Congo_Roads/Democratic Republic of Congo_Roads.shp", 
                     "Democratic Republic of Congo_Roads")
drc_roads <- spTransform(drc_roads, newcrs)
drc_roads_paved <- drc_roads[drc_roads@data$SURF_TYPE1=="Asphalt",]

# Republic of Congo
congo_roads <- readOGR("data/roads/Re of Congo_Roads/Republic of Congo_Roads.shp", 
                       "Republic of Congo_Roads")
congo_roads <- spTransform(congo_roads, newcrs)
congo_roads_paved <- congo_roads[congo_roads@data$SURF_TYPE1=="Asphalt",]

# Eritrea
eritrea_roads <- readOGR("data/roads/eritrea_roads/Eritrea_Roads.shp", 
                         "Eritrea_Roads")
eritrea_roads <- spTransform(eritrea_roads, newcrs)
eritrea_roads_paved <- eritrea_roads[eritrea_roads@data$SURFTYPE=="Paved",]

# Liberia
liberia_roads <- readOGR("data/roads/liberia_roads/Liberia_Roads.shp", 
                         "Liberia_Roads")
liberia_roads <- spTransform(liberia_roads, newcrs)
liberia_roads_paved <- liberia_roads[liberia_roads@data$SURFTYPE=="Paved",]

# Mozambique
mozambique_roads <- readOGR("data/roads/mozambique_roads/Mozambique_Roads.shp", 
                            "Mozambique_Roads")
mozambique_roads <- spTransform(mozambique_roads, newcrs)
mozambique_roads_paved <- mozambique_roads[mozambique_roads@data$PAVEDTYPE=="Paved",]

# Niger
niger_roads <- readOGR("data/roads/niger_roads/Niger_Roads.shp", 
                       "Niger_Roads")
niger_roads <- spTransform(niger_roads, newcrs)
niger_roads_paved <- niger_roads[niger_roads@data$SURF_TYPE1=="Asphalt",]

# North Sudan 
northsudan_roads <- readOGR("data/roads/north-sudan_roads/North Sudan_Roads.shp", 
                            "North Sudan_Roads")
northsudan_roads <- spTransform(northsudan_roads, newcrs)
northsudan_roads_paved <- northsudan_roads[northsudan_roads@data$SURFTYPE=="Paved",]

# Sierra Leone
sierraleone_roads <- readOGR("data/roads/sierra-leone_roads/Sierra Leone_Roads.shp", 
                             "Sierra Leone_Roads")
sierraleone_roads <- spTransform(sierraleone_roads, newcrs)
sierraleone_roads_paved <- sierraleone_roads[sierraleone_roads@data$SURFTYPE=="Paved",]

# South Africa
southafrica_roads <- readOGR("data/roads/south-africa_roads/South Africa_Roads.shp", 
                             "South Africa_Roads")
southafrica_roads <- spTransform(southafrica_roads, newcrs)
southafrica_roads_paved <- southafrica_roads[southafrica_roads@data$SURFTYPE=="Paved",]

# South Sudan
southsudan_roads <- readOGR("data/roads/south-sudan_roads/South Sudan_Roads.shp", 
                            "South Sudan_Roads")
southsudan_roads <- spTransform(southsudan_roads, newcrs)
southsudan_roads_paved <- southsudan_roads[southsudan_roads@data$SURFTYPE=="Paved",]

# Swaziland
swaziland_roads <- readOGR("data/roads/swaziland_roads/Swaziland_Roads.shp", 
                           "Swaziland_Roads")
swaziland_roads <- spTransform(swaziland_roads, newcrs)
swaziland_roads_paved <- swaziland_roads[swaziland_roads@data$SURFTYPE=="Paved",]

# Tanzania
tanzania_roads <- readOGR("data/roads/tanzania_roads/Tanzania_Roads.shp", 
                          "Tanzania_Roads")
tanzania_roads <- spTransform(tanzania_roads, newcrs)
tanzania_roads_paved <- tanzania_roads[tanzania_roads@data$SURFTYPE=="Paved",]

# Gambia
gambia_roads <- readOGR("data/roads/the-gambia_roads/The Gambia_Roads.shp", 
                        "The Gambia_Roads")
gambia_roads <- spTransform(gambia_roads, newcrs)
gambia_roads_paved <- gambia_roads[gambia_roads@data$SURFTYPE=="Paved",]

# Togo
togo_roads <- readOGR("data/roads/togo_roads/Togo_Roads.shp", 
                      "Togo_Roads")
togo_roads <- spTransform(togo_roads, newcrs)
togo_roads_paved <- togo_roads[togo_roads@data$SURF_TYPE1=="Asphalt",]

# Zimbabwe
zimbabwe_roads <- readOGR("data/roads/zimbabwe_roads/Zimbabwe_Roads.shp", 
                          "Zimbabwe_Roads")
zimbabwe_roads <- spTransform(zimbabwe_roads, newcrs)
zimbabwe_roads_paved <- zimbabwe_roads[zimbabwe_roads@data$SURFTYPE=="Paved",]

# Kenya
kenya_roads <- readOGR("data/roads/kenya_roads/Kenya_roads_version2.shp", 
                       "Kenya_roads_version2")
kenya_roads <- spTransform(kenya_roads, newcrs)
kenya_roads_paved <- kenya_roads[kenya_roads@data$SURFTYPE=="Paved",]

# Central African 
car_roads <- readOGR("data/roads/Central African Re_Roads/Central African Republic_Roads.shp", 
                     "Central African Republic_Roads")
car_roads <- spTransform(car_roads, newcrs)
car_roads_paved <- car_roads[car_roads@data$SURF_TYPE1=="Asphalt",]

# Djibouti 
dji_roads <- readOGR("data/roads/dji_roads/DJI_roads.shp", 
                     "DJI_roads")
dji_roads <- spTransform(dji_roads, newcrs)
dji_roads_paved <- dji_roads[dji_roads@data$SURF_TYPE1=="Asphalt",]

# Ethiopia 
ethiopia_roads <- readOGR("data/roads/ethiopia_roads/Ethiopia_Roads.shp", 
                          "Ethiopia_Roads")
ethiopia_roads <- spTransform(ethiopia_roads, newcrs)
ethiopia_roads_paved <- ethiopia_roads[ethiopia_roads@data$SURFTYPE=="Paved",]

# Gabon 
gabon_roads <- readOGR("data/roads/gabon_roads/Gabon_Roads.shp", 
                       "Gabon_Roads")
gabon_roads <- spTransform(gabon_roads, newcrs)
gabon_roads_paved <- gabon_roads[gabon_roads@data$SURF_TYPE1=="Asphalt",]

# Ghana
ghana_roads <- readOGR("data/roads/ghana_roads/Ghana_Roads.shp", 
                       "Ghana_Roads")
ghana_roads <- spTransform(ghana_roads, newcrs)
ghana_roads_paved <- ghana_roads[ghana_roads@data$SURF_TYPE1=="Flexible Asphalt",]

# Namibia
namibia_roads <- readOGR("data/roads/namibia_roads/Namibia_Roads.shp", 
                         "Namibia_Roads")
namibia_roads <- spTransform(namibia_roads, newcrs)
namibia_roads_paved <- namibia_roads[namibia_roads@data$SURFTYPE=="Paved",]

# Rwanda
rwanda_roads <- readOGR("data/roads/rwanda_roads/Rwanda_Roads.shp", 
                        "Rwanda_Roads")
rwanda_roads <- spTransform(rwanda_roads, newcrs)
rwanda_roads_paved <- rwanda_roads[rwanda_roads@data$SURF_TYPE1=="Asphalt",]

# Senegal
senegal_roads <- readOGR("data/roads/senegal_roads/Senegal_Roads.shp", 
                         "Senegal_Roads")
senegal_roads <- spTransform(senegal_roads, newcrs)
senegal_roads_paved <- senegal_roads[senegal_roads@data$SURF_TYPE1=="Asphalt",]

# Uganda
uganda_roads <- readOGR("data/roads/uganda_roads/Uganda_Roads.shp", 
                        "Uganda_Roads")
uganda_roads <- spTransform(uganda_roads, newcrs)
uganda_roads_paved <- uganda_roads[uganda_roads@data$SURFTYPE=="Paved",]

# Zambia
zambia_roads <- readOGR("data/roads/zambia_roads/Zambia_Roads.shp", 
                        "Zambia_Roads")
zambia_roads <- spTransform(zambia_roads, newcrs)
zambia_roads_paved <- zambia_roads[zambia_roads@data$SURFTYPE=="Paved",]


## Joining all the datasets
ab<-union(uganda_roads_paved, zambia_roads_paved)

road_datasets <- c(zambia_roads_paved, uganda_roads_paved, 
                   senegal_roads_paved, rwanda_roads_paved, 
                   namibia_roads_paved, ghana_roads_paved,
                   gabon_roads_paved, ethiopia_roads_paved, 
                   dji_roads_paved, kenya_roads_paved, 
                   zimbabwe_roads_paved, togo_roads_paved, 
                   gambia_roads_paved, tanzania_roads_paved, 
                   swaziland_roads_paved, southsudan_roads_paved, 
                   southafrica_roads_paved, sierraleone_roads_paved, 
                   northsudan_roads_paved, niger_roads_paved, 
                   mozambique_roads_paved, liberia_roads_paved, 
                   eritrea_roads_paved, congo_roads_paved, 
                   drc_roads_paved, cotedivoire_roads_paved, 
                   botswana_roads_paved, angola_roads_paved, 
                   mauritania_roads_paved, mali_roads_paved, 
                   malawi_roads_paved, burundi_roads_paved,
                   lesotho_roads_paved, guinea_roads_paved, 
                   cameroon_roads_paved, burkina_faso_roads_paved, 
                   benin_roads_paved, madagascar_roads_paved)

roads <- zambia_roads_paved
for (i in road_datasets){
  roads <- union(roads,i)
  print(i)
}


## Calculating the distance from the port to the closest paved road
distances_road <- c()
counter <- 0
for (i in row.names(africa@data)){
  
  cell <- africa[africa@data$ID==i,]
  distances_road[i] <- gDistance(cell, roads)
  
  counter <- counter+1
  print(counter/length(row.names(africa@data)))
}

africa@data$distances_road <- (distances_road)