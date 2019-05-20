## This file adds grid cell level data to the main dataset
# load("~/Dropbox/ports_ml/output/data_prep_2_tmp1.RData")


## Loading the main dataset
# load("/Users/sebastianellingsen/Dropbox/ports_ml/africa.Rda")
# load("/Users/sebastianellingsen/Dropbox/ports_ml/output/my_work_space_24.RData")


## Nighlights data
# lights <- raster("data/nightlights/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")
# lights_small <- aggregate(lights, 2)
# lights_small_projected <- projectRaster(lights_small,
#                                         crs = newcrs,
#                                         method = "bilinear")

lights_small_projected <- raster("lights_small_projected.grd")

## Population density 
# pop_density <- raster("data/population_density/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev10_2015_2pt5_min_tif/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev10_2015_2pt5_min.tif")
# pop_density <- aggregate(pop_density, 2)
# pop_density_projected <- projectRaster(pop_density,
#                                        crs = newcrs,
#                                        method = "bilinear")
# writeRaster(pop_density_projected, 
#             filename="pop_density_projected.grd", 
#             datatype='FLT4S', 
#             overwrite=TRUE)
pop_density_projected <- raster("pop_density_projected.grd")

## Precipitation and climate variables
bio = getData('worldclim', var='bio', res=2.5, lon=22.440823, lat=5.539446)

# BIO1 = Annual Mean Temperature
bio1 <- raster(bio, layer=1)   
bio1 <- aggregate(bio1, 2)
bio1_projected <- projectRaster(bio1, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
bio2 <- raster(bio, layer=2)   
bio2 <- aggregate(bio2, 2)
bio2_projected <- projectRaster(bio2, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO3 = Isothermality (BIO2/BIO7) (* 100)
bio3 <- raster(bio, layer=3)   
bio3 <- aggregate(bio3, 2)
bio3_projected <- projectRaster(bio3, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO4 = Temperature Seasonality (standard deviation *100)
bio4 <- raster(bio, layer=4)   
bio4 <- aggregate(bio4, 2)
bio4_projected <- projectRaster(bio4, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO5 = Max Temperature of Warmest Month
bio5 <- raster(bio, layer=5)   
bio5 <- aggregate(bio5, 2)
bio5_projected <- projectRaster(bio5, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO6 = Min Temperature of Coldest Month
bio6 <- raster(bio, layer=6)   
bio6 <- aggregate(bio6, 2)
bio6_projected <- projectRaster(bio6, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO7 = Temperature Annual Range (BIO5-BIO6)
bio7 <- raster(bio, layer=7)   
bio7 <- aggregate(bio7, 2)
bio7_projected <- projectRaster(bio7, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO8 = Mean Temperature of Wettest Quarter
bio8 <- raster(bio, layer=8)   
bio8 <- aggregate(bio8, 2)
bio8_projected <- projectRaster(bio8, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO9 = Mean Temperature of Driest Quarter
bio9 <- raster(bio, layer=9)   
bio9 <- aggregate(bio9, 2)
bio9_projected <- projectRaster(bio9, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO10 = Mean Temperature of Warmest Quarter
bio10 <- raster(bio, layer=10)   
bio10 <- aggregate(bio10, 2)
bio10_projected <- projectRaster(bio10, 
                                crs = newcrs, 
                                method = "bilinear")

# BIO11 = Mean Temperature of Coldest Quarter
bio11 <- raster(bio, layer=11)   
bio11 <- aggregate(bio11, 2)
bio11_projected <- projectRaster(bio11, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO12 = Annual Precipitation
bio12 <- raster(bio, layer=12)   
bio12 <- aggregate(bio12, 2)
bio12_projected <- projectRaster(bio12, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO13 = Precipitation of Wettest Month
bio13 <- raster(bio, layer=13)   
bio13 <- aggregate(bio13, 2)
bio13_projected <- projectRaster(bio13, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO14 = Precipitation of Driest Month
bio14 <- raster(bio, layer=14)   
bio14 <- aggregate(bio14, 2)
bio14_projected <- projectRaster(bio14, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO15 = Precipitation Seasonality (Coefficient of Variation)
bio14 <- raster(bio, layer=15)   
bio14 <- aggregate(bio14, 2)
bio14_projected <- projectRaster(bio14, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO16 = Precipitation of Wettest Quarter
bio15 <- raster(bio, layer=16)   
bio15 <- aggregate(bio15, 2)
bio15_projected <- projectRaster(bio15, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO17 = Precipitation of Driest Quarter
bio16 <- raster(bio, layer=17)   
bio16 <- aggregate(bio16, 2)
bio16_projected <- projectRaster(bio16, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO18 = Precipitation of Warmest Quarter
bio17 <- raster(bio, layer=18)   
bio17 <- aggregate(bio17, 2)
bio17_projected <- projectRaster(bio17, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO19 = Precipitation of Coldest Quarter
bio18 <- raster(bio, layer=19)   
bio18 <- aggregate(bio18, 2)
bio18_projected <- projectRaster(bio18, 
                                 crs = newcrs, 
                                 method = "bilinear")

# BIO20 = Precipitation of Coldest Quarter
bio19 <- raster(bio, layer=20)   
bio19 <- aggregate(bio19, 2)
bio19_projected <- projectRaster(bio19, 
                                 crs = newcrs, 
                                 method = "bilinear")
# Distance to mineral deposit
mines <- readOGR("data/mines/ofr20051294/ofr20051294.shp", 
                 "ofr20051294")
mines <- spTransform(mines, newcrs)


# Coffee suitabilty 
coffee <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/coffee/res03_crav6190l_silr_cof.tif")
coffee_projected <- projectRaster(coffee, 
                                  crs = newcrs, 
                                  method = "bilinear")

# Tea suitabilty 
tea <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/tea/res03_crav6190i_siir_tea.tif")
tea_projected <- projectRaster(tea, 
                               crs = newcrs, 
                               method = "bilinear")

# Tobacco suitabilty 
tobacco <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/tobacco/res03_crav6190l_silr_tob.tif")
tobacco_projected <- projectRaster(tobacco, 
                               crs = newcrs, 
                               method = "bilinear")

# Banana suitabilty 
banana <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/banana/res03_crav6190h_sihr_ban.tif")
banana_projected <- projectRaster(banana, 
                                   crs = newcrs, 
                                   method = "bilinear")

# Cacao suitabilty 
cacao <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/cacao/res03_crav6190h_sihr_coc.tif")
cacao_projected <- projectRaster(cacao, 
                                 crs = newcrs, 
                                 method = "bilinear")

# Citrus suitabilty 
citrus <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/citrus/res03_crav6190l_silr_cit.tif")
citrus_projected <- projectRaster(citrus, 
                                  crs = newcrs, 
                                  method = "bilinear")

# Cotton suitabilty 
cotton <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/cotton/res03_crav6190l_silr_cot.tif")
cotton_projected <- projectRaster(cotton, 
                                  crs = newcrs, 
                                  method = "bilinear")

# Palm oil suitabilty 
palm_oil <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/palm_oil/res03_crav6190l_silr_olp.tif")
palm_oil_projected <- projectRaster(palm_oil, 
                                  crs = newcrs, 
                                  method = "bilinear")

# Sugarcane suitabilty 
sugarcane <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/sugarcane/res03_crav6190h_sihr_suc.tif")
sugarcane_projected <- projectRaster(sugarcane, 
                                    crs = newcrs, 
                                    method = "bilinear")

# Soybean suitabilty 
soybean <- raster("/Users/sebastianellingsen/Dropbox/ports_ml/Data/crops/soybean/res03_crav6190i_siir_soy.tif")
soybean_projected <- projectRaster(soybean, 
                                     crs = newcrs, 
                                     method = "bilinear")

# Elevation data
elev <- raster("elev.grd")

# Landcover data 
# land_cover <- raster("data/landcover/Globcover2009_V2.3_Global_/GLOBCOVER_L4_200901_200912_V2.3.tif")
# land_cover <- projectRaster(land_cover,
#                             crs=newcrs)
# water_areas <- land_cover==210

## Averaging raster values at the grid cell level
lights_data <- c()        # Luminosity      
density_data <- c()       # Population density 
tri <- c()                # Terrain ruggedness 
bio1 <- c()        
bio2 <- c()      
bio3 <- c()       
bio4 <- c()       
bio5 <- c()       
bio6 <- c()      
bio7 <- c()      
bio8 <- c()      
bio9 <- c()      
bio10 <- c()      
bio11 <- c()     
bio12 <- c()     
bio13 <- c()      
bio14 <- c()       
bio15 <- c()     
bio16 <- c()       
bio17 <- c()     
bio18 <- c()      
bio19 <- c()       
slope <- c()
max_elev <- c()
min_elev <- c()
mean_elev <- c()
coffee_distance <- c()
tea_distance <- c()
banana_distance <- c()
tobacco_distance <- c()
cacao_distance <- c()
citrus_distance <- c()
cotton_distance <- c()
palm_oil_distance <- c()
sugarcane_distance <- c()
soybean_distance <- c()
coast_distance <- c()
mines_distance <- c()

counter <- 0

val <- function(layer, cell) {
  val <- values(mask(crop(layer, cell), cell))
  return(val)
}

for (i in row.names(africa@data)){
  cell <- africa[africa@data$ID==i,]
  
  ## Dependent variables
  lights_data[i] <- mean(val(lights_small_projected, cell), na.rm=TRUE)
  density_data[i] <- mean(val(pop_density_projected, cell), na.rm=TRUE)
  
  ## Terrain data
  ri <- tri(crop(elev, cell),
            exact = TRUE,
            s = 3)
  tri[i] <- mean(val(ri, cell), na.rm=TRUE)
    
  slope_cell <- terrain(crop(elev, cell),
                        opt='slope',
                        unit='radians',
                        neighbors=8)

  slope[i] <- mean(val(slope_cell, cell), na.rm=TRUE)
  
  mean_elev[i] <- mean(val(elev, cell), na.rm=TRUE)
  
  min_elev[i] <- min(val(elev, cell), na.rm=TRUE)
  
  max_elev[i] <- max(val(elev, cell), na.rm=TRUE)
  
  ## Distance to mineral deposit
  mines_distance[i] <- gDistance(cell, mines)
  
  ## Distance to coastline
  coast_distance[i] <- gDistance(cell, coastline10)
  
  ## Climate data
  bio1[i] <- mean(val(bio1_projected, cell), na.rm=TRUE)
  
  bio2[i] <- mean(val(bio2_projected, cell), na.rm=TRUE)
  
  bio3[i] <- mean(val(bio3_projected, cell), na.rm=TRUE)
 
  bio4[i] <- mean(val(bio4_projected, cell), na.rm=TRUE)
  
  bio5[i] <- mean(val(bio5_projected, cell), na.rm=TRUE)

  bio6[i] <- mean(val(bio6_projected, cell), na.rm=TRUE)
  
  bio7[i] <- mean(val(bio7_projected, cell), na.rm=TRUE)
  
  bio8[i] <- mean(val(bio8_projected, cell), na.rm=TRUE)

  bio9[i] <- mean(val(bio9_projected, cell), na.rm=TRUE)

  bio10[i] <- mean(val(bio10_projected, cell), na.rm=TRUE)
  
  bio11[i] <- mean(val(bio11_projected, cell), na.rm=TRUE)
  
  bio12[i] <- mean(val(bio12_projected, cell), na.rm=TRUE)

  bio13[i] <- mean(val(bio13_projected, cell), na.rm=TRUE)
  
  bio14[i] <- mean(val(bio14_projected, cell), na.rm=TRUE)

  bio15[i] <- mean(val(bio15_projected, cell), na.rm=TRUE)
  
  bio16[i] <- mean(val(bio16_projected, cell), na.rm=TRUE)

  bio17[i] <- mean(val(bio17_projected, cell), na.rm=TRUE)

  bio18[i] <- mean(val(bio18_projected, cell), na.rm=TRUE)
  
  bio19[i] <- mean(val(bio19_projected, cell), na.rm=TRUE)

  ## Crop suitability data
  
  coffee_distance[i] <- mean(val(coffee_projected, cell), na.rm=TRUE)
  
  tea_distance[i] <- mean(val(tea_projected, cell), na.rm=TRUE)
  
  banana_distance[i] <- mean(val(banana_projected, cell), na.rm=TRUE)
  
  tobacco_distance[i] <- mean(val(tobacco_projected, cell), na.rm=TRUE)
  
  cacao_distance[i] <- mean(val(cacao_projected, cell), na.rm=TRUE)
  
  citrus_distance[i] <- mean(val(citrus_projected, cell), na.rm=TRUE)
  
  cotton_distance[i] <- mean(val(cotton_projected, cell), na.rm=TRUE)
  
  palm_oil_distance[i] <- mean(val(palm_oil_projected, cell), na.rm=TRUE)
  
  sugarcane_distance[i] <- mean(val(sugarcane_projected, cell), na.rm=TRUE)
  
  soybean_distance[i] <- mean(val(soybean_projected, cell), na.rm=TRUE)
  
  # Progress
  counter <- counter+1
  print(counter/length(row.names(africa@data)))
}

## Joining and preparing datasets 
       
# Outcomes
africa@data$lights <- lights_data
africa@data$density <- density_data

# Controls
africa@data$tri <- tri
africa@data$bio1 <- bio1
africa@data$bio2 <- bio2
africa@data$bio3 <- bio3
africa@data$bio4 <- bio4
africa@data$bio5 <- bio5
africa@data$bio5 <- bio5
africa@data$bio7 <- bio7
africa@data$bio8 <- bio8
africa@data$bio9 <- bio9
africa@data$bio10 <- bio10
africa@data$bio11 <- bio11
africa@data$bio12 <- bio12
africa@data$bio13 <- bio13
africa@data$bio14 <- bio14
africa@data$bio15 <- bio15
africa@data$bio16 <- bio16
africa@data$bio17 <- bio17
africa@data$bio18 <- bio18
africa@data$bio19 <- bio19
africa@data$tri <- tri
africa@data$slope <- slope
africa@data$max_elev <- max_elev
africa@data$min_elev <- min_elev
africa@data$mean_elev <- mean_elev
africa@data$mine_dis <- mines_distance
africa@data$coast_dis <- coast_distance
africa@data$coffee_dis <- coffee_distance
africa@data$tea_dis <- tea_distance
africa@data$banana_dis <- banana_distance
africa@data$tob_dis <- tobacco_distance
africa@data$cacao_dis <- cacao_distance
africa@data$citrus_dis <- citrus_distance
africa@data$cotton_dis <- cotton_distance
africa@data$palm_oil_dis <- palm_oil_distance
africa@data$sugarcane_dis <- sugarcane_distance
africa@data$soybean_dis <- soybean_distance

## Adding longitude and latitude
africa@data$long <- coordinates(africa)[,1]
africa@data$lat <- coordinates(africa)[,2]









