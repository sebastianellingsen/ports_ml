## This file prepares the city dataset of cities for analysis

countries_list <- c("Chile", 
                    "Bolivia", 
                    "Peru", 
                    "Argentina", 
                    "Uruguay",   
                    "Ecuador", 
                    "Colombia", 
                    "Paraguay", 
                    "Venezuela", 
                    "Panama",
                    "El Salvador", 
                    "Honduras", 
                    "Costa Rica", 
                    "Guatemala", 
                    "Mexico", 
                    "Nicaragua", 
                    "Cuba", 
                    "Dominican Republic")

## This section generates a panel of cities at the grid-cell level at 50 year 
## intervals 
cities <- read_csv("data/cities_sa/cities_sa.csv") %>% 
  filter(!is.na(year), country!="Brazil", !is.na(pop), pop>1000) %>% 
  dplyr::select(city_ascii, lat, lng, country, year)

years <- c("1500", "1550", "1600", "1650", "1700", "1750", "1800",
           "1850", "1900", "1950", "2000")

crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Dataset containing the spanish empire 
sa <- south_america[south_america@data$ccode!="Brazil", ]

for(i in years){ 
  
  # Generating dataset for each period
  cutoff <- as.numeric(i)
  cities_tmp <- cities %>% filter(year <= cutoff)
  
  # Make each dataset a spatial dataframe 
  coords          <-  cbind(cities_tmp$lng, cities_tmp$lat)
  sp              <-  SpatialPoints(coords)
  cities_tmp      <-  SpatialPointsDataFrame(coords, cities_tmp)
  crs(cities_tmp) <- crs
  cities_tmp      <- spTransform(cities_tmp, crs(sa))
  
  
  # Assign the name to each spatial dataset
  name <- paste("cities", i, sep="_")
  assign(name, cities_tmp)
}

# Function to find the number of cities per grid-cell
cities_cell <- function(x, points){
  cell   <- sa[sa@data$ID==x, ]
  l      <- over(cell, points, byid=TRUE, returnList = TRUE)
  return(length(l[[1]]$city_ascii))
}

# Finding the vectors counting cities per grid-cells for each year 
sa@data$city_1500 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1500))
sa@data$city_1550 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1550))
sa@data$city_1600 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1600))
sa@data$city_1650 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1650))
sa@data$city_1700 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1700))
sa@data$city_1750 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1750))
sa@data$city_1800 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1800))
sa@data$city_1850 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1850))
sa@data$city_1900 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1900))
sa@data$city_1950 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1950))
sa@data$city_2000 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_2000))


# Defining one dataframe for each year and joining these
sa1500 <- sa@data %>% dplyr::select(-c(city_1550, city_1600, city_1650, 
                                       city_1700, city_1750, city_1800, 
                                       city_1850, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1500) %>% mutate(year = 1500)

sa1550 <- sa@data %>% dplyr::select(-c(city_1500, city_1600, city_1650, 
                                       city_1700, city_1750, city_1800, 
                                       city_1850, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1550) %>% mutate(year = 1550)

sa1600 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1650, 
                                       city_1700, city_1750, city_1800, 
                                       city_1850, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1600) %>% mutate(year = 1600)

sa1650 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1700, city_1750, city_1800, 
                                       city_1850, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1650) %>% mutate(year = 1650)

sa1700 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1650, city_1750, city_1800, 
                                       city_1850, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1700) %>% mutate(year = 1700)

sa1750 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1650, city_1700, city_1800, 
                                       city_1850, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1750) %>% mutate(year = 1750)

sa1800 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1650, city_1700, city_1750, 
                                       city_1850, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1800) %>% mutate(year = 1800)

sa1850 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1650, city_1700, city_1750, 
                                       city_1800, city_1900, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1850) %>% mutate(year = 1850)


sa1900 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1650, city_1700, city_1750, 
                                       city_1800, city_1850, city_1950,  
                                       city_2000)) %>% 
  rename(city = city_1900) %>% mutate(year = 1900)

sa1950 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1650, city_1700, city_1750, 
                                       city_1800, city_1850, city_1900,  
                                       city_2000)) %>% 
  rename(city = city_1950) %>% mutate(year = 1950)

sa2000 <- sa@data %>% dplyr::select(-c(city_1500, city_1550, city_1600, 
                                       city_1650, city_1700, city_1750, 
                                       city_1800, city_1850, city_1900,  
                                       city_1950)) %>% 
  rename(city = city_2000) %>% mutate(year = 2000)

## Final panel dataset 
sa_panel <- rbind(sa1500, sa1550, sa1600, sa1650, sa1700, sa1750, sa1800,
                  sa1850, sa1900, sa1950, sa2000) %>% 
  dplyr::select(ID, year, city, ccode, states)

## Add data on the change in market access 
ma_data1 <- south_america@data %>%  
  full_join(sa_panel, by=c("ID", "ccode", "states"))

write.csv(ma_data1, "data/ma_data1.csv", row.names = FALSE)




