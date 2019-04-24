
#################################################
## This file prepares the tables for the paper ##
#################################################

# load("/Users/sebastianellingsen/Dropbox/ports_ml/africa.Rda")
# load("/Users/sebastianellingsen/Dropbox/ports_ml/africa_cities.Rda")

## Loading packages and setting the theme
if (!require(pacman)) install.packages("pacman")
p_load(sandwich, lmtest, AER, stargazer, lfe)


# # Landlocked
# landlocked <- c("ZWE", "BWA", "BFA", "BDI", "TCD", "ETH", "LSO", "MWI", "MLI", 
#                 "NER", "RWA", "CAF", "SDS", "UGA", "ZMB", "ZWE") %>% 
#   as.data.frame() %>% 
#   mutate(landlocked=1) %>% 
#   rename("ISO"=".") %>% 
#   full_join(africa_cities1,by="ISO") %>% 
#   mutate(landlocked=ifelse(is.na(landlocked),0,landlocked))
# 
# ## Grid cell level analysis
# africa_coast <- africa[(africa@data$distance_coast)<150, ]
# africa_coast@data$distance_coast <- exp(africa_coast@data$distance_coast)
# africa_coast@data$distance_coast2 <- (africa_coast@data$distance_coast)^2
# africa_coast@data$distance_coast3 <- (africa_coast@data$distance_coast)^3
#   

  



africa_coast1 <- africa_coast %>% filter(!is.na(density_data))
  
  
e1 <- residuals(felm(log(1+density_data)~ 
       log(distance_coast+1)+  
       log(distance_coast2+1)+ 
       mines_distance|
       country_var |
       0|
       country_var,
     data=africa_coast1))

e2 <- residuals(felm(ldistance_pport~ 
                       log(distance_coast+1)+  
                       log(distance_coast2+1)+ 
                       mines_distance|
                       country_var |
                       0|
                       country_var,
                     data=africa_coast1))

africa_coast1$e1 <- e1
africa_coast1$e2 <- e2

ggplot(data=africa_coast1, aes(x=e1, y=e2))+
  xlab("") + ylab("")+ggtitle("Effect of port suitability on port location (residualized)")+
  # geom_point(shape=1, alpha=0.7)
  stat_summary_bin(shape=1,fun.y='mean', alpha=0.7, bins=600, size=1.4, geom='point')+
  theme(plot.title = element_text(size = 7))


ggplot(data=africa_coast1, aes(x=ldistance_pport, y=log(1+density_data)))+
  xlab("") + ylab("")+ggtitle("Effect of port suitability on port location (residualized)")+
  # geom_point(shape=1, alpha=0.7)
  stat_summary_bin(shape=1,fun.y='mean', alpha=0.7, bins=1000, size=1.4, geom='point')+
  theme(plot.title = element_text(size = 7))





  



# 
# ## City level analysis
# africa_cities1 <-  africa_cities %>% mutate(distance_coast2=log(1+distance_coast)^2,
#                                             distance_coast3=distance_coast^3) %>% 
#   mutate(g <- log(as.numeric(as.character(pop2010)))-log(as.numeric(as.character(pop1950))))
# #%>% 
# #  filter(distance_actual>100)
# 
# m3 <- lm(g ~ log(distance_predicted+1)+factor(ISO),
#          data=africa_cities1)
# 
# m4 <- lm(g ~ log(distance_predicted+1)+factor(ISO) +distance_coast,
#          data=africa_cities1)
# 
# m5 <- lm(g ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2,
#          data=africa_cities1)
# 
# iv1 <- ivreg(g~factor(ISO) + log(distance_actual+1)|
#                factor(ISO) + distance_predicted, data=africa_cities1)
# 
# iv2 <- ivreg(g~factor(ISO) + log(distance_actual+1)+distance_coast+distance_coast2|
#                factor(ISO) + log(distance_predicted+1)+distance_coast+distance_coast2, data=africa_cities1)
# 
# results_table1 <- stargazer(m3, m4,m5,iv1,iv2, type="text", 
#                   star.char = c("*"),
#                   dep.var.caption = "",
#                   style="io",
#                   dep.var.labels.include = FALSE,
#                   header = FALSE,
#                   column.labels   = c("OLS", "IV"),
#                   column.separate = c(2, 2),
#                   notes = "I make this look good!",
#                   notes.append = FALSE,
#                   model.names = FALSE,
#                   omit = c("Constant", "country_var", "distance_coast", "distance_coast2",
#                            "ISO"),
#                   covariate.labels = c("$d(i,\\widehat{Ports})$", "$d(i,Ports)$"),
#                   font.size="scriptsize",
#                   omit.stat = c("rsq", "f", "ser"),
#                   add.lines = list(c("Controls","","$\\checkmark$","$\\checkmark$","",
#                                      "$\\checkmark$","$\\checkmark$")))
# note.latex <- "\\multicolumn{5}{c} {\\parbox[c]{8cm}{\\tiny{\\textit{Notes:} Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long and interesting comment.}}} \\\\"
# results_table1[grepl("Note",results_table1)] <- note.latex
# cat (results_table1, sep = "\n")
# 
# 
# 
# 
# ## Heterogenous effects
# # Instrumental variable
# iv1 <- ivreg(g~factor(country_code) + log(distance_actual+1)*log(trade)+distance_coast|
#                factor(country_code) + log(distance_predicted+1)*log(trade)+distance_coast, data=africa_cities_trade)
# 
# 
# iv2 <- ivreg(g~factor(ISO) + log(distance_actual+1)*landlocked+distance_coast|
#              factor(ISO) + log(distance_predicted+1)*landlocked+distance_coast, 
#              data=landlocked)
# 
# 
# iv3 <- ivreg(g~factor(ISO) + log(distance_actual+1)*log(distance_coast+1)|
#                factor(ISO) + log(distance_predicted+1)*log(distance_coast+1), 
#              data=africa_cities1)
# 
# iv4 <- ivreg(g~factor(country_code) + log(distance_actual+1)*polity|
#                factor(country_code) + log(distance_predicted+1)*polity, 
#              data=africa_cities_polity)
# 
# results_table1 <- stargazer(iv1,iv2,iv3,iv4, type="text",
#                             #se = list(rse1),
#                             star.char = c(""),
#                             dep.var.caption = "",
#                             style="io",
#                             dep.var.labels.include = FALSE,
#                             header = FALSE,
#                             #column.labels   = c("OLS", "IV"),
#                             #column.separate = c(2, 2),
#                             notes = "I make this look good!",
#                             notes.append = FALSE,
#                             model.names = FALSE,
#                             #omit = c("Constant", "country_code", "distance_coast", "distance_coast2",
#                             #         "ISO"),
#                             keep = c("distance_actual"),
#                             covariate.labels = c("$d(i,Ports)$"),
#                             font.size="scriptsize",
#                             omit.stat = c("rsq", "f", "ser"),
#                             add.lines = list(c("Country FE","$\\checkmark$","$\\checkmark$",
#                                                "$\\checkmark$","$\\checkmark$")))
# 
# 
# 
# ## OLS
# m1 <- lm(g~factor(country_code) + log(distance_actual+1)*log(trade)+distance_coast, 
#          data=africa_cities_trade)
# 
# m2 <- lm(g~factor(ISO) + log(distance_actual+1)*landlocked+distance_coast, 
#          data=landlocked)
# 
# m3 <- lm(g~factor(ISO) + log(distance_actual+1)*log(distance_coast+1), 
#          data=africa_cities1)
# 
# m4 <- lm(g~factor(country_code) + log(distance_actual+1)*polity, 
#          data=africa_cities_polity)
# 
# 
# results_table1 <- stargazer(m1,m2,m3,m4, type="text",
#                             #se = list(rse1),
#                             star.char = c(""),
#                             dep.var.caption = "",
#                             style="io",
#                             dep.var.labels.include = FALSE,
#                             header = FALSE,
#                             #column.labels   = c("OLS", "IV"),
#                             #column.separate = c(2, 2),
#                             notes = "I make this look good!",
#                             notes.append = FALSE,
#                             model.names = FALSE,
#                             omit = c("Constant", "country_code", "distance_coast", "distance_coast2",
#                                      "ISO"),
#                             covariate.labels = c("$d(i,Ports)$"),
#                             font.size="scriptsize",
#                             omit.stat = c("rsq", "f", "ser"),
#                             add.lines = list(c("Controls","","$\\checkmark$","$\\checkmark$","",
#                                                "$\\checkmark$","$\\checkmark$")))
# 
# 
# # Placebo
# 
# ## City level analysis
# africa_cities1 <-  africa_cities %>% mutate(distance_coast2=log(1+distance_coast)^2,
#                                             distance_coast3=distance_coast^3) %>% 
#   mutate(zambia=ifelse(ISO=="ZMB",1,0)) %>% 
#   mutate(pop2010=(as.numeric(as.character(pop2010))),
#          pop2000=(as.numeric(as.character(pop2000))),
#          pop1970=(as.numeric(as.character(pop1970))),
#          pop1950=(as.numeric(as.character(pop1950))),
#          pop1980=(as.numeric(as.character(pop1980))),
#          Dens2015=(as.numeric(as.character(Dens2015))),
#          pop1990=(as.numeric(as.character(pop1990))),
#          pop1960=as.numeric(as.character(pop1960))) %>% 
#   mutate(g1970=log(pop1970-pop1960),
#          g1980=log(pop1980-pop1970),
#          g1990=log(pop1990-pop1980),
#          g2000=log(pop2000-pop1990),
#          g2010=log(pop2010-pop2000)) %>% 
#   filter(is.finite(g2010)) 
# 
# m3 <- lm(g2010 ~ log(distance_predicted+1)*zambia+factor(ISO), 
#          data=africa_cities1)
# 
# m4 <- lm(g2010 ~ log(distance_predicted+1)*zambia+factor(ISO) +distance_coast, 
#          data=africa_cities1)
# 
# m5 <- lm(g2010 ~ log(distance_actual+1)*zambia+factor(ISO) +distance_coast+ distance_coast2, 
#          data=africa_cities1)
# 
# results_table1 <- stargazer(m3, m4,m5, type="text", 
#                             star.char = c(""),
#                             dep.var.caption = "",
#                             style="io",
#                             dep.var.labels.include = FALSE,
#                             header = FALSE,
#                             column.labels   = c("OLS", "IV"),
#                             column.separate = c(2, 2),
#                             notes = "I make this look good!",
#                             notes.append = FALSE,
#                             model.names = FALSE,
#                             omit = c("Constant", "country_var", "distance_coast", "distance_coast2",
#                                      "ISO"),
#                             covariate.labels = c("$d(i,\\widehat{Ports})$", "$d(i,Ports)$"),
#                             font.size="scriptsize",
#                             omit.stat = c("rsq", "f", "ser"),
#                             add.lines = list(c("Controls","","$\\checkmark$","$\\checkmark$","",
#                                                "$\\checkmark$","$\\checkmark$")))
# note.latex <- "\\multicolumn{5}{c} {\\parbox[c]{8cm}{\\tiny{\\textit{Notes:} Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long and interesting comment.}}} \\\\"
# results_table1[grepl("Note",results_table1)] <- note.latex
# cat (results_table1, sep = "\n")
# 
# # 1960s lower, same in 1970s, lower in 1980s, same in 1990s, same in 2000s
# 
# # 1975: tanzam railway
# # 1979: trade diverted west through angola until the civil war
# 
# # three natural experiments:
# # building of the tanzam railway
# # trade diverted west through angola until the civil war
# # commodity price boom in 2000s
# 
# 
# 
# 
# 
# 
# ## City level analysis
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#                                             distance_coast3=distance_coast^3) %>% 
#   filter(pop1970!=0) %>% 
#   mutate(pop2010=log(1+as.numeric(as.character(pop2010))),
#          pop2000=log(1+as.numeric(as.character(pop2000))),
#          pop1970=log(1+as.numeric(as.character(pop1970))),
#          pop1950=log(1+as.numeric(as.character(pop1950))),
#          pop1980=log(1+as.numeric(as.character(pop1980))),
#          Dens2015=log(1+as.numeric(as.character(Dens2015))),
#          pop1990=log(1+as.numeric(as.character(pop1990))),
#          pop1960=log(1+as.numeric(as.character(pop1960))))
# 
# # Cities 1950
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#          distance_coast3=distance_coast^3) %>% 
#   filter(pop1950!=0) %>% 
#   mutate(pop1950=log(1+as.numeric(as.character(pop1950))))
# 
# m1950 <-  lm(pop1950 ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2, 
#          data=africa_cities1)
# 
# # Cities 1960
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#          distance_coast3=distance_coast^3) %>% 
#   filter(pop1960!=0) %>% 
#   mutate(pop1960=log(1+as.numeric(as.character(pop1960))))
# 
# m1960 <-  lm(pop1960 ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2, 
#              data=africa_cities1)
# 
# # Cities 1970
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#          distance_coast3=distance_coast^3) %>% 
#   filter(pop1970!=0) %>% 
#   mutate(pop1970=log(1+as.numeric(as.character(pop1970))))
# 
# m1970 <-  lm(pop1970 ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2, 
#              data=africa_cities1)
# 
# # Cities 1980
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#          distance_coast3=distance_coast^3) %>% 
#   filter(pop1980!=0) %>% 
#   mutate(pop1980=log(1+as.numeric(as.character(pop1980))))
# 
# m1980 <-  lm(pop1980 ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2, 
#              data=africa_cities1)
# 
# # Cities 1990
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#          distance_coast3=distance_coast^3) %>% 
#   filter(pop1990!=0) %>% 
#   mutate(pop1990=log(1+as.numeric(as.character(pop1990))))
# 
# m1990 <-  lm(pop1990 ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2, 
#              data=africa_cities1)
# 
# # Cities 2000
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#          distance_coast3=distance_coast^3) %>% 
#   filter(pop2000!=0) %>% 
#   mutate(pop2000=log(1+as.numeric(as.character(pop2000))))
# 
# m2000 <-  lm(pop2000 ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2, 
#              data=africa_cities1)
# 
# # Cities 2010
# africa_cities1 <-  africa_cities %>% 
#   mutate(distance_coast2=log(1+distance_coast)^2,
#          distance_coast3=distance_coast^3) %>% 
#   filter(pop2010!=0) %>% 
#   mutate(pop2010=log(1+as.numeric(as.character(pop2010))))
# 
# m2010 <-  lm(pop2010 ~ log(distance_actual+1)+factor(ISO) +distance_coast+ distance_coast2, 
#              data=africa_cities1)
# 
# 
# library(broom)
# 
# m2010_df <- tidy(m2010) %>% filter(term=="log(distance_actual + 1)")
# m2000_df <- tidy(m2000) %>% filter(term=="log(distance_actual + 1)")
# m1990_df <- tidy(m1990) %>% filter(term=="log(distance_actual + 1)")
# m1980_df <- tidy(m1980) %>% filter(term=="log(distance_actual + 1)")
# m1970_df <- tidy(m1970) %>% filter(term=="log(distance_actual + 1)")
# m1960_df <- tidy(m1960) %>% filter(term=="log(distance_actual + 1)")
# m1950_df <- tidy(m1950) %>% filter(term=="log(distance_actual + 1)")
# 
# result <- m2010_df %>% 
#   full_join( m2000_df) %>%
#   full_join(m1990_df) %>% 
#   full_join(m1980_df) %>% 
#   full_join(m1970_df) %>% 
#   full_join(m1960_df) %>% 
#   full_join(m1950_df) 
#   
# year <- c("2010","2000","1990","1980","1970","1960","1950")
# result$year <- year
# 
# result <- result %>% filter(year!=2010)
# ggplot(data=result, mapping=aes(x=year, y=estimate, group=1)) +
#   geom_errorbar(aes(x = year, ymin = estimate - 1.96*std.error, ymax = estimate+1.96*std.error), width = .3) +
#   xlab("") + ylab("") + geom_point(size = 1, shape = 21, fill = "black") +
#   geom_hline(yintercept=0, color="black", linetype="dashed", size=0.3, alpha = 0.5)+ 
#   theme(strip.background = element_blank(), strip.placement = "outside")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# results_table1 <- stargazer(m1950, m1960, m1970, m1980,m1990,m2000,m2010,
#                             type="text", 
#                             star.char = c("*"),
#                             dep.var.caption = "",
#                             style="io",
#                             dep.var.labels.include = FALSE,
#                             header = FALSE,
#                             column.labels   = c("OLS", "IV"),
#                             column.separate = c(2, 2),
#                             notes = "I make this look good!",
#                             notes.append = FALSE,
#                             model.names = FALSE,
#                             omit = c("Constant", "country_var", "distance_coast", "distance_coast2",
#                                      "ISO"),
#                             covariate.labels = c("$d(i,\\widehat{Ports})$", "$d(i,Ports)$"),
#                             font.size="scriptsize",
#                             omit.stat = c("rsq", "f", "ser"),
#                             add.lines = list(c("Controls","","$\\checkmark$","$\\checkmark$","",
#                                                "$\\checkmark$","$\\checkmark$")))
# 
# 
# note.latex <- "\\multicolumn{5}{c} {\\parbox[c]{8cm}{\\tiny{\\textit{Notes:} Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long Logistic regression. Dependent variable: an indicator varible ... AND Some very long and interesting comment.}}} \\\\"
# results_table1[grepl("Note",results_table1)] <- note.latex
# cat (results_table1, sep = "\n")
# 
# 









## Heterogenous effects


africa_1 <-  africa@data %>% mutate(mine=ifelse(mines_distance==0,1,0)) %>% 
  group_by(country_var) %>% 
  add_tally(mine)



africa1 <- africa[!is.na(africa@data$density_data),]


m1 <- (lm(data=africa1@data,  log(density_data+1)~ ldistance_pport + 
           log(distance_coast+1)+  log(distance_coast2+1)+ 
           factor(country_var)))

fit <- fitted(m1, africa1@data)
africa1@data$fit <- fit

tanzania <- africa1[africa1@data$ADMIN=="Tanzania",]


p1 <- tm_shape(tanzania) +
  tm_fill(col="fit", n=100, palette=viridis(256)) +
  tm_layout(frame=FALSE, legend.show=FALSE,bg.color="white",
            main.title="Min. distance to predicted port site", main.title.size=0.7)



