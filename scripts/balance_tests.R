## This file runs balance tests for the main specification

## Preparing data for the balance test 
p_values <- c()
t_stat <- c()
p_values <- c()
mean1 <- c()
mean0 <- c()
variables <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
               "13", "14", "15", "16", "17", "18", "19", "20")

africa_coast <- africa@data %>% 
  mutate(pport_site=ifelse(ldistance_pport==0,1,0)) %>% 
  filter(distance_coast==0) %>% 
  dplyr::select("tri", "slope", "bio1", "bio2", "bio3", "bio4", "bio5", "bio7", 
                "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14",
                "bio15", "bio16", "bio17", "bio18", "bio19", "long", 
                "lat", "pport_site", "country_var")



## Adding residuals to the dataframe
res <- c()
residuals <- lapply(1:20, 
                    function(i) resid(lm(africa_coast[,i+1] ~ africa_coast[,"country_var"]))) 
residuals <- sapply(1:20, 
                    function(i) c(res, residuals[[i]]))
africa_coast1 <- cbind(africa_coast,residuals)



## Balance test
for (i in variables){
  
  port_site1 <- africa_coast1 %>% filter(pport_site==1)
  port_site0 <- africa_coast1 %>% filter(pport_site==0)
  
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


## Table without Bonferroni correction
balance_test <- cbind(variables,
                      round(mean1, digits = 2),
                      round(mean0, digits = 2),
                      round(mean1-mean0, digits = 2),
                      round(t_stat, digits = 2),
                      round(p_values, digits = 3)) %>% 
  as.data.frame() %>% 
  dplyr::select(-variables)

rownames(balance_test)[1] <- "Terrain Rugedness Index"
rownames(balance_test)[2] <- "Average slope"
rownames(balance_test)[3] <- "Annual Mean Temperature"
rownames(balance_test)[4] <- "Mean Diurnal Range"
rownames(balance_test)[5] <- "Isothermality*100"
rownames(balance_test)[6] <- "Temperature Seasonality"
rownames(balance_test)[7] <- "Max Temperature of Warmest Month"
rownames(balance_test)[8] <- "Min Temperature of Coldest Month"
rownames(balance_test)[9] <- "Temperature Annual Range"
rownames(balance_test)[10] <- "Mean Temperature of Wettest Quarter"
rownames(balance_test)[11] <- "Mean Temperature of Driest Quarter"
rownames(balance_test)[12] <- "Mean Temperature of Warmest Quarter"
rownames(balance_test)[13] <- "Mean Temperature of Coldest Quarter"
rownames(balance_test)[14] <- "Annual Precipitation"
rownames(balance_test)[15] <- "Precipitation of Wettest Month"
rownames(balance_test)[16] <- "Precipitation of Driest Month"
rownames(balance_test)[17] <- "Precipitation Seasonality"
rownames(balance_test)[18] <- "Precipitation of Wettest Quarter"
rownames(balance_test)[19] <- "Precipitation of Driest Quarter"
rownames(balance_test)[20] <- "Precipitation of Warmest Quarter"

colnames(balance_test)[1] <- "Mean 1"
colnames(balance_test)[2] <- "Mean 0"
colnames(balance_test)[3] <- "Diff."
colnames(balance_test)[4] <- "t-stat."
colnames(balance_test)[5] <- "p-values"

# Writing out the table
balance_test_table <- xtable(balance_test, 
                             include.rownames=TRUE, 
                             latex.environments = "left")

comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(balance_test_table))
comment$command  <- c(paste("\\hline \n", 
                            "\\multicolumn{6}{c}{\\parbox{8.7cm}{\\textit{Note}: The 
                            table displays the results from tests of equality 
                            for variables along the coastline for areas with 
                            and without predicted ports. The variables have
                            been demeaned using country averages. Assumptions
                            about equal variances is not imposed.
                            Temperature annual range = Max temperature of warmest month/
                            min temperature of coldest month. Mean diurnal                                                                 range = (mean of monthly (max temp - min temp).                                                                Isothermality = Mean diurnal range/ Temperature 
                            annual range.}}  \n",
                            sep = ""))

print(xtable(balance_test_table,
             align="lrrrrr"),
      add.to.row = comment,
      hline.after = c(-1, 0), size="\\tiny")  # indicates rows that will contain hlines (the last one was defined up there)



## Table with Bonferroni correction
p_values <- p.adjust(p_values, method = "bonferroni", n = length(p_values))

balance_test_bf <- cbind(variables,
                      round(mean1, digits = 2),
                      round(mean0, digits = 2),
                      round(mean1-mean0, digits = 2),
                      round(t_stat, digits = 2),
                      round(p_values, digits = 3)) %>% 
  as.data.frame() %>% 
  dplyr::select(-variables)

rownames(balance_test_bf)[1] <- "Terrain Rugedness Index"
rownames(balance_test_bf)[2] <- "Average slope"
rownames(balance_test_bf)[3] <- "Annual Mean Temperature"
rownames(balance_test_bf)[4] <- "Mean Diurnal Range"
rownames(balance_test_bf)[5] <- "Isothermality*100"
rownames(balance_test_bf)[6] <- "Temperature Seasonality"
rownames(balance_test_bf)[7] <- "Max Temperature of Warmest Month"
rownames(balance_test_bf)[8] <- "Min Temperature of Coldest Month"
rownames(balance_test_bf)[9] <- "Temperature Annual Range"
rownames(balance_test_bf)[10] <- "Mean Temperature of Wettest Quarter"
rownames(balance_test_bf)[11] <- "Mean Temperature of Driest Quarter"
rownames(balance_test_bf)[12] <- "Mean Temperature of Warmest Quarter"
rownames(balance_test_bf)[13] <- "Mean Temperature of Coldest Quarter"
rownames(balance_test_bf)[14] <- "Annual Precipitation"
rownames(balance_test_bf)[15] <- "Precipitation of Wettest Month"
rownames(balance_test_bf)[16] <- "Precipitation of Driest Month"
rownames(balance_test_bf)[17] <- "Precipitation Seasonality"
rownames(balance_test_bf)[18] <- "Precipitation of Wettest Quarter"
rownames(balance_test_bf)[19] <- "Precipitation of Driest Quarter"
rownames(balance_test_bf)[20] <- "Precipitation of Warmest Quarter"

colnames(balance_test_bf)[1] <- "Mean 1"
colnames(balance_test_bf)[2] <- "Mean 0"
colnames(balance_test_bf)[3] <- "Diff."
colnames(balance_test_bf)[4] <- "t-stat."
colnames(balance_test_bf)[5] <- "p-values"


# Writing out the table
balance_test_table_bf <- xtable(balance_test_bf, 
                             include.rownames=TRUE, 
                             latex.environments = "left")

comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(balance_test_table_bf))
comment$command  <- c(paste("\\hline \n",  
                            "\\multicolumn{6}{c}{\\parbox{8.7cm}{Note: your footnote, caption or whatever. your footnote, caption or whatever.your footnote, caption or whatever.}}  \n",
                            sep = ""))

print(xtable(balance_test_table_bf,
             align="lrrrrr"),
      add.to.row = comment,
      hline.after = c(-1, 0), size="\\tiny")  



