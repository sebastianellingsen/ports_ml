## This file runs balance tests for the main specification to see whether port 
## and non-port areas are similar in terms of topographic and climatic 
## variables. 

balance_data <- south_america@data %>% filter(coast_ds==0) %>% 
  mutate(pport=ifelse(ds_pprt==0, 1, 0),
         year=2010)

climate   <- colnames(balance_data)[3:17]
geography <- c(colnames(balance_data)[28:30], colnames(balance_data)[32:33])
crops     <- colnames(balance_data)[23:26]
cultural  <- colnames(balance_data)[43:44]



regression_test <- function(variables){
  
  regression <- lapply(variables, 
                  function(i) (felm(data=balance_data,  
                                   balance_data[,i] ~ pport*slng_tm|
                                     states|
                                     0|
                                     states)%>% tidy())[3,])  %>%
    bind() %>%
    mutate(term = variables)
  
  return(regression)
}

regressions <- rbind(regression_test(climate),
                     regression_test(geography),
                     regression_test(crops),
                     regression_test(cultural)) %>% data.frame()






