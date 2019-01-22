library(readxl)
library(naniar)
library(countrycode)

# Loading data 
excel_sheets("data/mpd2018.xlsx")
excel_sheets("data/Trade_of_Goods.xlsx")

# Trade 
trade_data <- read_excel("data/Trade_of_Goods.xlsx", skip=5) %>% 
  select(-"Base Year", -"Scale") %>% 
  replace_with_na_all(condition=~.x=="...") %>% 
  gather("year", "trade",2:167) %>% 
  filter(year>=1950, year<=2013, !is.na(trade)) %>% 
  mutate(trade=as.numeric(trade), year=as.numeric(year))

country_code <- sapply(trade_data$Country, 
                       function(x) countrycode(x, 'country.name', 'iso3c'))
trade_data$country_code <- country_code 
trade_data <- trade_data[!is.na(trade_data$country_code),]

# Madison data
econ_data <- read_excel("data/mpd2018.xlsx", sheet="Full data") %>% 
  filter(!is.na(cgdppc)) %>% filter(year>=1940) %>% 
  group_by(country) %>% 
  mutate(m_year=min(year)) %>% 
  filter(m_year<=1940) 
 
# Data on ports and harbors
harbor_data <- countries_list@data %>% mutate(country=SOVEREIGNT, country_code=ISO_A3) %>% 
  select(n_harbors, harbors, country, area, country_code)

# Combining datasets
combined <- inner_join(trade_data, harbor_data, by = "country_code") %>% 
  #inner_join(econ_data, by = c("country", "year")) %>% 
  filter(n_harbors>0) %>% 
  mutate(n_harbors=(n_harbors), gdp=(trade), 
         area=log(area), p_high=ifelse(n_harbors>30,1,0)) %>% 
  group_by(p_high, year) %>% 
  summarise(m_gdp=mean(gdp), n=n()) 

print(ggplot(combined,aes(year,m_gdp, shape=as.factor(p_high))) + geom_point())



#harbor_data$country[!(harbor_data$country_code %in% trade_data$country_code)]


  


%>% 
  #inner_join(econ_data, by = c("country", "year")) %>% 
  filter(n_harbors>0) %>% 
  mutate(n_harbors=log(n_harbors), gdp=log(trade), 
                                 area=log(area), p_high=ifelse(n_harbors>=2.197,1,0)) %>% 
  #select(-i_bm, -countrycode, -i_cig) %>% 
  
  
  
  
  group_by(p_high, year) %>% 
  summarise(m_gdp=mean(gdp))

  mutate(m_year=min(year)) %>% 
  filter(m_year<=1960) %>% 























%>% 
  group_by(p_high, year) %>% 
  summarise(m_gdp=mean(gdp))




#ggplotRegression(lm(gdp ~ n_harbors + area, data = combined)) + xlab("log(Harbors)") + ylab("log(Ports)")

library(plm)
library(broom)
lm_fixed <- plm(gdp ~ n_harbors*factor(year), data = combined, index = c("country.x"), model = "within")
#summary(lm_fixed)

a <- tidy(lm_fixed, conf.int = TRUE)

a <- a[which(a$term=="n_harbors:factor(year)1951"):which(a$term=="n_harbors:factor(year)2013"),]

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

