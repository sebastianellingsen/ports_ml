## This file runs balance tests for the main specification to see whether port 
## and non-port areas are similar in terms of topographic and climatic 
## variables. 

## Climate and precipitation
# BIO1  = Annual Mean Temperature 
# BIO2  = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3  = Isothermality (BIO2/BIO7) (* 100)
# BIO4  = Temperature Seasonality (standard deviation *100)
# BIO5  = Max Temperature of Warmest Month
# BIO6  = Min Temperature of Coldest Month
# BIO7  = Temperature Annual Range (BIO5-BIO6)
# BIO8  = Mean Temperature of Wettest Quarter
# BIO9  = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter
# BIO20 = Precipitation of Coldest Quarter


## loading data
bio = getData('worldclim', var='bio', res=2.5, lon=22.440823, lat=5.539446)

for(i in 1:20) { 
  
  k <- projectRaster(aggregate(raster(bio, layer=i), 2), 
                     crs = newcrs, 
                     method = "bilinear")
  assign(paste("bio", i, sep = "") , k)
  print(i)
}
 
# Coffee suitabilty 
coffee <- raster("data/crops/coffee/res03_crav6190l_silr_cof.tif")
coffee <- projectRaster(coffee, 
                        crs = newcrs, 
                        method = "bilinear")

# Tobacco suitabilty 
tobacco <- raster("data/crops/tobacco/res03_crav6190l_silr_tob.tif")
tobacco <- projectRaster(tobacco, 
                         crs = newcrs, 
                         method = "bilinear")

# Cotton suitabilty 
cotton <- raster("data/crops/cotton/res03_crav6190l_silr_cot.tif")
cotton <- projectRaster(cotton, 
                        crs = newcrs, 
                        method = "bilinear")

# Sugarcane suitabilty 
sugarcane <- raster("data/crops/sugarcane/res03_crav6190h_sihr_suc.tif")
sugarcane <- projectRaster(sugarcane, 
                           crs = newcrs, 
                           method = "bilinear")

# locations of mineral depposits
mines <- readOGR("data/mines/ofr20051294/ofr20051294.shp", 
                 "ofr20051294")
mines <- spTransform(mines, newcrs)

# Elevation data
elev <- raster("data/prepared_rasters/elev.grd")




## Extract information from a buffer around each port
rasters                <- c(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, 
                            bio9, bio10, bio11, bio12, bio13, bio14, bio15, 
                            bio16, bio17, bio18, bio19, bio20, coffee, tobaco, 
                            cotton, sugar)

a                      <- gBuffer(pports, width = 5, byid = T)

extracting_raster_info <- function(x){
  
  b <- raster::extract(x, a)
  return(sapply(1:length(b), 
                function(x) min(b[[x]],
                            na.rm = TRUE)))
}

output <- lapply(rasters, function(x) extracting_raster_info(x))




















## Balance test
for (i in 1:18){
  
  port_site1 <- africa_coast1 %>% filter(pport_site.y==1)
  port_site0 <- africa_coast1 %>% filter(pport_site.y==0)
  
  # Running the test
  v1 <- port_site1[[i]]
  v2 <- port_site0[[i]]
  test <- t.test(v1,v2,
                 var.equal = FALSE)
  
  # Test stats
  mean1 <- c(mean1, test[["estimate"]][1])
  mean0 <- c(mean0, test[["estimate"]][2])
  p_values <- c(p_values, test[["p.value"]])
  t_stat <- c(t_stat, test[["statistic"]][["t"]])
}

balance_test <- cbind(round(mean1, digits = 2),
                      round(mean0, digits = 2),
                      round(mean1-mean0, digits = 2),
                      round(t_stat, digits = 2),
                      round(p_values, digits = 3)) %>% 
  as.data.frame() 
rownames(balance_test) <- c("Tri", "Slope", "Max. elev."," Min elev.", "Coffee", 
                            "Citrus", "Tea", "Banana", "Tobacco", "Cacao", 
                            "Cotton", "Palm oil", "Sugar cane", "Soybean",
                            "Annual Mean Temp.", "Annual Mean Prec.", 
                            "Temp. seasonality", "Prec. seasonality")

balance_test <- stargazer(balance_test,
                style="io",
                column.separate = c(2, 2),
                summary = FALSE,
                type="latex",
                header = FALSE,
                float = F)

# Remove $
balance_test <- gsub("\\$\\$\\-\\$", "\\$\\-", balance_test)



year <- c(1500,
          1903,
1948,
1948,
1821,
1999,
1838,
1862,
1894,
1953,
1966,
1890,
1891,
2003,
1928,
1905,
1912,
1876,
1889,
1890,
1880,
1964,
1974,
1922,
1883,
1960,
1948,
1950,
1928,
1962,
1965,
1913,
1866,
1986,
1879,
1870,
1925,
1969,
1976,
1652,
1840,
1967,
1967,
1970,
1970,
1905)

year1 <- year[year>=1945]

# areas more or less suitable would have evolved in the same way 
# were it not for the port construction
























