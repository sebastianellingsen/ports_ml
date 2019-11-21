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
  filter(!is.na(year), country!="Brazil", !is.na(pop), pop>10000) %>% 
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

write.csv(ma_data1, "data/ma_data2.csv", row.names = FALSE)






## add 25 year periods 
cities <- read_csv("data/cities_sa/cities_sa.csv") %>% 
  filter(!is.na(year), country!="Brazil", !is.na(pop), pop>10000) %>% 
  dplyr::select(city_ascii, lat, lng, country, year)

years <- c("1500", "1520", "1540", "1560", "1580",
           "1600", "1620", "1640", "1660", "1680",
           "1700", "1720", "1740", "1760", "1780",
           "1800", "1820", "1840", "1860", "1880",
           "1900", "1920", "1940", "1960", "1980",
           "2000")

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
sa@data$city_1520 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1520))
sa@data$city_1540 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1540))
sa@data$city_1560 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1560))
sa@data$city_1580 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1580))

sa@data$city_1600 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1600))
sa@data$city_1620 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1620))
sa@data$city_1640 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1640))
sa@data$city_1660 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1660))
sa@data$city_1680 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1680))

sa@data$city_1700 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1700))
sa@data$city_1720 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1720))
sa@data$city_1740 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1740))
sa@data$city_1760 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1760))
sa@data$city_1780 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1780))

sa@data$city_1800 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1800))
sa@data$city_1820 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1820))
sa@data$city_1840 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1840))
sa@data$city_1860 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1860))
sa@data$city_1880 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1880))

sa@data$city_1900 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1900))
sa@data$city_1920 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1920))
sa@data$city_1940 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1940))
sa@data$city_1960 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1960))
sa@data$city_1980 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1980))

sa@data$city_2000 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_2000))


# Defining one dataframe for each year and joining these
## 16th century
sa1500 <- sa@data %>% dplyr::select(ID, ccode, states, city_1500) %>% 
  dplyr::rename(city = city_1500) %>% mutate(year = 1500)

sa1520 <- sa@data %>% dplyr::select(ID, ccode, states, city_1520) %>% 
  dplyr::rename(city = city_1520) %>% mutate(year = 1520)

sa1540 <- sa@data %>% dplyr::select(ID, ccode, states, city_1540) %>% 
  dplyr::rename(city = city_1540) %>% mutate(year = 1540)

sa1560 <- sa@data %>% dplyr::select(ID, ccode, states, city_1560) %>% 
  dplyr::rename(city = city_1560) %>% mutate(year = 1560)

sa1580 <- sa@data %>% dplyr::select(ID, ccode, states, city_1580) %>% 
  dplyr::rename(city = city_1580) %>% mutate(year = 1580)

## 17th century
sa1600 <- sa@data %>% dplyr::select(ID, ccode, states, city_1600) %>% 
  dplyr::rename(city = city_1600) %>% mutate(year = 1600)

sa1620 <- sa@data %>% dplyr::select(ID, ccode, states, city_1620) %>% 
  dplyr::rename(city = city_1620) %>% mutate(year = 1620)

sa1640 <- sa@data %>% dplyr::select(ID, ccode, states, city_1640) %>% 
  dplyr::rename(city = city_1640) %>% mutate(year = 1640)

sa1660 <- sa@data %>% dplyr::select(ID, ccode, states, city_1660) %>% 
  dplyr::rename(city = city_1660) %>% mutate(year = 1660)

sa1680 <- sa@data %>% dplyr::select(ID, ccode, states, city_1680) %>% 
  dplyr::rename(city = city_1680) %>% mutate(year = 1680)

## 18th century
sa1700 <- sa@data %>% dplyr::select(ID, ccode, states, city_1700) %>% 
  dplyr::rename(city = city_1700) %>% mutate(year = 1700)

sa1720 <- sa@data %>% dplyr::select(ID, ccode, states, city_1720) %>% 
  dplyr::rename(city = city_1720) %>% mutate(year = 1720)

sa1740 <- sa@data %>% dplyr::select(ID, ccode, states, city_1740) %>% 
  dplyr::rename(city = city_1740) %>% mutate(year = 1740)

sa1760 <- sa@data %>% dplyr::select(ID, ccode, states, city_1760) %>% 
  dplyr::rename(city = city_1760) %>% mutate(year = 1760)

sa1780 <- sa@data %>% dplyr::select(ID, ccode, states, city_1780) %>% 
  dplyr::rename(city = city_1780) %>% mutate(year = 1780)

## 19th century
sa1800 <- sa@data %>% dplyr::select(ID, ccode, states, city_1800) %>% 
  dplyr::rename(city = city_1800) %>% mutate(year = 1800)

sa1820 <- sa@data %>% dplyr::select(ID, ccode, states, city_1820) %>% 
  dplyr::rename(city = city_1820) %>% mutate(year = 1820)

sa1840 <- sa@data %>% dplyr::select(ID, ccode, states, city_1840) %>% 
  dplyr::rename(city = city_1840) %>% mutate(year = 1840)

sa1860 <- sa@data %>% dplyr::select(ID, ccode, states, city_1860) %>% 
  dplyr::rename(city = city_1860) %>% mutate(year = 1860)

sa1880 <- sa@data %>% dplyr::select(ID, ccode, states, city_1880) %>% 
  dplyr::rename(city = city_1880) %>% mutate(year = 1880)

## 20th century
sa1900 <- sa@data %>% dplyr::select(ID, ccode, states, city_1900) %>% 
  dplyr::rename(city = city_1900) %>% mutate(year = 1900)

sa1920 <- sa@data %>% dplyr::select(ID, ccode, states, city_1920) %>% 
  dplyr::rename(city = city_1920) %>% mutate(year = 1920)

sa1940 <- sa@data %>% dplyr::select(ID, ccode, states, city_1940) %>% 
  dplyr::rename(city = city_1940) %>% mutate(year = 1940)

sa1960 <- sa@data %>% dplyr::select(ID, ccode, states, city_1960) %>% 
  dplyr::rename(city = city_1960) %>% mutate(year = 1960)

sa1980 <- sa@data %>% dplyr::select(ID, ccode, states, city_1980) %>% 
  dplyr::rename(city = city_1980) %>% mutate(year = 1980)

sa2000 <- sa@data %>% dplyr::select(ID, ccode, states, city_2000) %>% 
  dplyr::rename(city = city_2000) %>% mutate(year = 2000)

## Final panel dataset 
sa_panel <- rbind(sa1500, sa1520, sa1540, sa1560, sa1580, 
                  sa1600, sa1620, sa1640, sa1660, sa1680, 
                  sa1700, sa1720, sa1740, sa1760, sa1780, 
                  sa1800, sa1820, sa1840, sa1860, sa1880, 
                  sa1900, sa1920, sa1940, sa1960, sa1980, 
                  sa2000) %>% 
  dplyr::select(ID, year, city, ccode, states)

## Add data on the change in market access 
ma_data <- south_america@data %>%  
  full_join(sa_panel, by=c("ID", "ccode", "states"))

write.csv(ma_data, "data/ma_data_20.csv", row.names = FALSE)





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
  filter(!is.na(year), country!="Brazil", !is.na(pop)) %>% 
  dplyr::select(city_ascii, lat, lng, country, year)

years <- c("1680", "1700", "1720", "1740", "1760", "1780", "1800",
           "1820", "1840", "1860", "1880")

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
sa@data$city_1500 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1680))
sa@data$city_1550 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1700))
sa@data$city_1600 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1720))
sa@data$city_1650 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1740))
sa@data$city_1700 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1760))
sa@data$city_1750 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1780))
sa@data$city_1800 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1800))
sa@data$city_1850 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1820))
sa@data$city_1900 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1840))
sa@data$city_1950 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1860))
sa@data$city_2000 <- sapply(sa@data$ID, function(x) cities_cell(x, cities_1880))


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

write.csv(ma_data1, "data/ma_data_large.csv", row.names = FALSE)




