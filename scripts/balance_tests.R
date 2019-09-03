## This file runs balance tests for the main specification to see whether port 
## and non-port areas are similar in terms of topographic and climatic 
## variables. 

balance_data <- south_america@data %>% filter(coast_ds==0) %>% 
  mutate(pport=ifelse(ds_pprt==0, 1, 0),
         year=2010)

climate   <- c(colnames(balance_data)[3:4], colnames(balance_data)[6:8],  
               colnames(balance_data)[14:17])
geography <- c(colnames(balance_data)[28:30], colnames(balance_data)[32:33])
crops     <- colnames(balance_data)[23:26]
cultural  <- c(colnames(balance_data)[41], colnames(balance_data)[43:44])

regression_test <- function(variables){
  
  regression <- lapply(variables, 
                  function(i) (felm(data=balance_data,  
                                   balance_data[,i] ~ port_p*slng_tm|
                                     states|
                                     0|
                                     states)%>% broom::tidy())[3,])  %>%
                bind() %>%
                mutate(term = variables)
  
  return(regression)
}


regressions <- rbind(regression_test(climate),
                     regression_test(geography),
                     regression_test(cultural)) %>% data.frame()
regressions <- regressions %>% dplyr::select(-term)

rownames <- c("Annual Mean Temperature",
              "Mean Diurnal Range",
              "Temperature Seasonality",
              "Max Temperature of Warmest Month",
              "Min Temperature of Coldest Month",
              "Annual Precipitation",
              "Precipitation of Wettest Month",
              "Precipitation of Driest Month",
              "Precipitation Seasonality",
              "Elevation",
              "Terrain Ruggedness",
              "Slope",
              "Longitude",
              "Latitude,",
              "Population density 1492",
              "Pre-hispanic city",
              "Archeological sites")

rownames(regressions) <- rownames

regressions <- regressions %>% stargazer(summary=F,
                                         header = F,
                                         type = "latex",
                                         digits = 3,
                                         covariate.labels=c("",
                                                            "Estimate",
                                                            "Se.",
                                                            "t-stat.",
                                                            "p-val."))

regressions <- gsub("\\$\\$\\-\\$", "\\$\\-", regressions)

