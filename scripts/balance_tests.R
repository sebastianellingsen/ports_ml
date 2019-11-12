## This file runs balance tests for the main specification to see whether port 
## and non-port areas are similar in terms of topographic and climatic 
## variables. 

balance_data <- south_america@data %>% filter(coast_ds<=15) %>% 
  mutate(pport=ifelse(dist_pport<=15, 1, 0))

climate   <- colnames(balance_data)[7:25]
geography <- colnames(balance_data)[c(28:30, 67, 69,70)]
crops     <- colnames(balance_data)[31:39]
cultural  <- colnames(balance_data)[50:51]
distances <- colnames(balance_data)[44:47]

regression_test <- function(variables){
  
  regression <- lapply(variables, 
                  function(i) (felm(data=balance_data,  
                                   balance_data[,i] ~ pport|
                                     states|
                                     0|
                                     states)%>% broom::tidy()))  %>%
                bind() %>%
                mutate(term = variables)
  
  return(regression)
}

regressions <- rbind(regression_test(climate),
                     regression_test(geography),
                     regression_test(crops),
                     regression_test(cultural),
                     regression_test(distances)) %>% data.frame() %>% 
  mutate(sig = ifelse(p.value<=0.1, '*', '-'))
regressions <- regressions %>% dplyr::select(-term)


rownames <- c('Annual Mean Temperature',
              'Mean Diurnal Range', 
              'Isothermality', 
              'Temperature Seasonality',
              'Max Temperature of Warmest Month',
              'Min Temperature of Coldest Month',
              'Temperature Annual Range',
              'Mean Temperature of Wettest Quarter',
              'Mean Temperature of Driest Quarter',
              'Mean Temperature of Warmest Quarter',
              'Mean Temperature of Coldest Quarter',
              'Annual Precipitation',
              'Precipitation of Wettest Month',
              'Precipitation of Driest Month',
              'Precipitation Seasonality',
              'Precipitation of Wettest Quarter',
              'Precipitation of Driest Quarter',
              'Precipitation of Warmest Quarter',
              'Precipitation of Coldest Quarter',
              "Elevation",
              "Terrain Ruggedness",
              "Slope",
              'Mineral Deposit',
              'Dist. Lake',
              'Dist. River',
              'Banana',
              'Coffee',
              'Tobacco',
              'Cotton',
              'Wheat',
              'Tea',
              'Sugarcane',
              'Cacao',
              'Maize',
              "Population density 1492",
              "Archeological sites",
              'Dist. Potosi',
              'Dist. Mexico City',
              'Dist. Quito',
              'Dist. Callao')

rownames(regressions) <- rownames

regressions <- regressions %>% stargazer(summary=F,
                                         header = F,
                                         type = "latex",
                                         digits = 3,
                                         covariate.labels=c("Dep. Var.",
                                                            "Estimate",
                                                            "Se.",
                                                            "t-stat.",
                                                            "p-val.",
                                                            'Sign.'))

regressions <- gsub("\\$\\$\\-\\$", "\\$\\-", regressions)

break1 <- "\\textit{Climate} &  &  &  & & \\\\"
break2 <- "\\textit{Geography} &  &  &  & &\\\\"
break3 <- "\\textit{Culture} &  &  &  & &\\\\"

regressions <- append(regressions,break1, after=9)
regressions <- append(regressions,break2, after=29)
regressions <- append(regressions,break3, after=42)

regressions[4] <- "\\tiny"
regressions[5] <- "\\begin{tabular}{@{\\extracolsep{5pt}} lrrrrr}"
regressions[6] <- ""
regressions[7] <- "\\hlineB{2.5}"
regressions[9] <- "\\hline"

break2 <- "\\tiny"
regressions <- append(regressions,break2, after=0)

cat (regressions, sep = "\n")




## Plotting the balance test
balance_data_scaled <- balance_data %>% mutate_each_(funs(scale(.) %>% 
                                                            as.vector), 
                                    vars=c(c(climate, geography, crops, 
                                             cultural, distances)))


regression_test <- function(variables){
  
  regression <- lapply(variables, 
                       function(i) (felm(data=balance_data_scaled,  
                                         balance_data_scaled[,i] ~ pport|
                                           states|
                                           0|
                                           states)%>% broom::tidy()))   %>%
    bind() %>%
    mutate(term = variables)
  
  return(regression)
}

# Climate variables
regressions <- rbind(regression_test(climate),
                     regression_test(geography),
                     regression_test(crops),
                     regression_test(cultural),
                     regression_test(distances)) %>% data.frame()  
regressions$term <- rownames
regressions$term <- factor(regressions$term, 
                           levels = regressions$term[order(regressions$estimate)])


## Balance test plots 
residual_plot <- function(var){
  
  balance_data_scaled_tmp <- balance_data_scaled %>% filter(!is.na(var))
  
  ## Obtaining regression coefficient
  coef <- felm(data=balance_data_scaled,  
               var ~ pport|
               states|
               0|
               states)[["coefficients"]][1]
  
  ## Calculating the residuals
  m <- felm(data=balance_data_scaled_tmp,  
            var~ 1|
              states|
              0|
              states)
  
  m1_res <- m[["residuals"]]
  m <- felm(data=balance_data_scaled_tmp,  
            pport~ 1|
              states|
              0|
              states)
  m2_res <- m[["residuals"]]
  
  residuals <- cbind(m1_res, m2_res, balance_data_scaled_tmp$pop) %>% 
    data.frame()

  return(ggplot(data = residuals, aes(x=pport, y = var, size=V3))+
    geom_point(alpha=0.3, show.legend = F, col='blue')+
    scale_size(range = c(0, 4))+
    geom_smooth(method='lm', se=F, col='black', size=0.3, show.legend = F)+
    geom_smooth(method = "lm", mapping = aes(weight = V3),
                color = "black", show.legend = F, se=F,linetype='dashed', size=0.3)+
    xlab('Pred. Port (residualized)')+    
    # geom_text(aes(x = -.5, y = 0.5, label = coef), parse = TRUE)+
    theme(plot.title = element_text(size = 0.1),text = element_text(size=5))+
      scale_y_continuous( limits = c(-3,3), expand = c(0,0)) +
      scale_x_continuous( limits = c(-1,1), expand = c(0,0)))

}


p1  <- residual_plot(balance_data_scaled$bio1)      + ggtitle('Temp.')           + theme(plot.title = element_text(size = 8)) + ylab('Temp. (residualized)')
p2  <- residual_plot(balance_data_scaled$bio12)     + ggtitle('Rain.')           + theme(plot.title = element_text(size = 8)) + ylab('Rain. (residualized)')
p3  <- residual_plot(balance_data_scaled$rivers)    + ggtitle('Dist. River')     + theme(plot.title = element_text(size = 8)) + ylab('Dist. River (residualized)')
p4  <- residual_plot(balance_data_scaled$lakes)     + ggtitle('Dist. Lake')      + theme(plot.title = element_text(size = 8)) + ylab('Dist. Lake (residualized)')
p5  <- residual_plot(balance_data_scaled$sugarcane) + ggtitle('Sugarcane')      + theme(plot.title = element_text(size = 8)) + ylab('Sugarcane (residualized)')
p6  <- residual_plot(balance_data_scaled$mine)      + ggtitle('Dist. Min. Dep.') + theme(plot.title = element_text(size = 8)) + ylab('Dist. Min. Dep. (residualized)')
p8  <- residual_plot(balance_data_scaled$sites)     + ggtitle('Arch. Site')      + theme(plot.title = element_text(size = 8)) + ylab('Arch. Site (residualized)')
p9  <- residual_plot(balance_data_scaled$slope)     + ggtitle('Slope')           + theme(plot.title = element_text(size = 8)) + ylab('Slope (residualized)')
p10 <- residual_plot(balance_data_scaled$elev)      + ggtitle('Elev.')           + theme(plot.title = element_text(size = 8)) + ylab('Elev. (residualized)')
p11 <- residual_plot(balance_data_scaled$sites)     + ggtitle('Cotton')          + theme(plot.title = element_text(size = 8)) + ylab('Cotton (residualized)')

ggarrange(p1, p3, p4, p5, p6,p8,p9, p11, ncol = 4, nrow = 2)


