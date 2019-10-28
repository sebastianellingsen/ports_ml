## This file splits the data and fits the final model  

## loading packages
if (!require(pacman)) install.packages("pacman")
p_load(ranger, 
       Metrics, 
       broom, 
       rsample, 
       tidyverse, 
       stargazer, 
       caret)

## Preparing the data 
pred_dataframe <- read_csv("output/pred_dataframe.csv") %>% dplyr::select(-river_dist) 



## Predicting the outcome
## Splitting the data to training, testing
data_split <- initial_split(pred_dataframe, 0.75)
training_data <- training(data_split)
testing_data <- testing(data_split)

## Cross validation folds
cv_split <- vfold_cv(training_data, v = 5)
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)), validate = map(splits, ~testing(.x)))

## Fitting the model
cv_models <- cv_data %>%
  mutate(forest_model = map(train, ~ranger(formula= y~., data=.x, num.trees = 1000, mtry = 1)))

cv_prep <- cv_models %>%
  mutate(validate_actual = map(validate, ~.x$y)) %>%
  mutate(predicted = map2(forest_model, validate, ~predict(.x,.y)$predictions))




## Predicting
if (!require(pacman)) install.packages("pacman")
p_load(ranger,
       Metrics,
       broom,
       rsample,
       tidyverse,
       stargazer,
       caret)

## Predicting the outcome
## Splitting the data to training, testing
data_split <- initial_split(pred_dataframe, 0.75)
training_data <- training(data_split)
testing_data <- testing(data_split)

## Cross validation folds
cv_split <- vfold_cv(training_data, v = 5)
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)), validate = map(splits, ~testing(.x)))

## Fitting the model
cv_models <- cv_data %>%
  mutate(forest_model = map(train, ~ranger(formula= y~min_elev+max_elev+mean_elev, data=.x, num.trees = 6000, mtry = 3)))

cv_prep <- cv_models %>%
  mutate(validate_actual = map(validate, ~.x$y)) %>%
  mutate(predicted = map2(forest_model, validate, ~predict(.x,.y)$predictions))

## Validating the fit of the model
#dta$prediction <- ifelse(pred>=0.3,1,0)
confusion_matrix <- map2(cv_prep$validate_actual, cv_prep$predicted, ~table(.x,.y))
c_matrix <- Reduce(`+`, confusion_matrix) %>% data.frame
colnames(c_matrix) <- c("Predicted", "Actual", "Cases")
c_matrix %>% stargazer(type="text", summary=FALSE, rownames = FALSE)

# confusionMatrix(confusion_matrix)

# prediction1 <- predict(model1, training_data)$predictions-1
# 
# 
## ROC curves
library(ROCR)

training_data$y <- as.numeric(training_data$y)

## Fitting the full model to the training data
model1 <- ranger(formula= as.numeric(y)~.,
                 data=training_data,
                 num.trees = 1,
                 mtry = 1)
model2 <- ranger(formula= as.numeric(y)~.,
                 data=training_data,
                 num.trees = 10,
                 mtry = 2)
model3 <- ranger(formula= as.numeric(y)~.,
                 data=training_data,
                 num.trees = 1000,
                 mtry = 3)

prediction1 <- predict(model1, testing_data)$predictions-1
prediction2 <- predict(model2, testing_data)$predictions-1
prediction3 <- predict(model3, testing_data)$predictions-1

pred1 <- prediction(prediction1, testing_data$y)
perf1 <- performance(pred1, "tpr", "fpr")
pred2 <- prediction(prediction2, testing_data$y)
perf2 <- performance(pred2, "tpr", "fpr")
pred3 <- prediction(prediction3, testing_data$y)
perf3 <- performance(pred3, "tpr", "fpr")

plot(perf1, col="green")
plot(perf2, add=TRUE, col="red")
plot(perf3, add=TRUE, col="blue")
abline(a=0, b= 1,col="grey", lty=2)




## The confusion matrix with rf
model <- ranger(formula= as.numeric(y)~.,
                data=training_data,
                num.trees = 1000,
                mtry = 3)
prediction <- predict(model, testing_data)$predictions
prediction <- ifelse(prediction>0.5,1,0)

confusion_matrix <- table(prediction, testing_data$y)
confusionMatrix(confusion_matrix)







# ## The confusion matrix with ols
training_data$y <- (training_data$y)

model <- lm(formula= as.numeric(y)~., data=training_data)
m_fit <- predict(model, testing_data)
prediction <- ifelse(m_fit>0.5,1,0)

confusion_matrix <- table(prediction, testing_data$y)
confusionMatrix(confusion_matrix)

pred1 <- prediction(m_fit, testing_data$y)
perf1 <- performance(pred1, "tpr", "fpr")

# 
# model <- ranger(formula= as.factor(y)~.,
#                 data=training_data,
#                 num.trees = 5000,
#                 mtry = 3,
#                 importance="impurity")
# 
# ctable2 <- as.data.frame.matrix(cmatrix2$table)
# stargazer(ctable2, summary = FALSE, type="text")
# 
# 
# 
# 
# 
# ctable1 <- as.data.frame.matrix(cmatrix1$table)
# ctable2 <- as.data.frame.matrix(cmatrix2$table)
# 
# c_table <- cbind(ctable1,ctable2) %>% as.data.frame()
# 
# stargazer(c_table,
#           column.labels   = c("Good", "Better"),
#           column.separate = c(2, 2),
#           title="Rf model", 
#           summary = FALSE, 
#           type="latex", 
#           header = FALSE)



library(vip)
var_imp <- vip(model)
#impurity/binomial?


model <- ranger(formula= as.factor(y)~ .,
                data=training_data,
                num.trees = 1000,
                mtry = 3,
                importance="impurity")


library(vip)

var_imp <- vip(model)

df <- cbind(importance = as.numeric(var_imp[["data"]][["Importance"]]),
            variable   = as.factor(var_imp[["data"]][["Variable"]])) %>% 
  as.data.frame() 

ggplot(data=df,aes(y=reorder(variable, importance), x=importance))+
  geom_point()+ylab("")+xlab("Variable Importance") +
  scale_y_discrete(labels=c("8" = "Dist. River", '5' = 'Slope',
                            '7' = 'Min. Elevation',
                            '3' = 'Max Elevation',
                            '2' = 'P. Elevation',
                            '1' = 'Shelter',
                            '9' = 'P. Ruggednes',
                            '4' = 'Mean Elevation',
                            '6' = 'Mean Ruggednes'))

  
  



# Plot model
plot(model)
# 
# 
# # save.image(file = "output/data_prep_2_tmp1.RData")
# # save(pred_dataframe, file = "pred_dataframe_prediction.RData")
# 
# 
# ## Fitting the model
# model3 <- ranger(formula= y~.,
#                  data=training_data,
#                  num.trees = 1000,
#                  mtry = 5)
# 
# 
# 


if (!require(pacman)) install.packages("pacman")
p_load(ranger,
       Metrics,
       broom,
       rsample,
       tidyverse,
       stargazer,
       caret)

## simple test of prediction
pred_dataframe1 <- pred_dataframe[sample(nrow(pred_dataframe)),] 

pred_dataframe1 <- pred_dataframe1 %>% 
  dplyr::mutate(mean_slope2 = mean_slope^2,
                mean_slope3 = mean_slope^3,
                mean_tri2 = mean_tri^2,
                mean_tri3 = mean_tri^3,
                min_elev2 = min_elev^2,
                min_elev3 = min_elev^3,
                max_elev2 = max_elev^2,
                max_elev3 = max_elev^3,
                mean_elev2 = mean_elev^2,
                mean_elev3 = mean_elev^3,
                elevation_p2 = elevation_p^2,
                elevation_p3 = elevation_p^3,
                tri_p2 = tri_p^2,
                tri_p3 = tri_p^3,
                slope_p2 = slope_p^2,
                slope_p3 = slope_p^3,
                len2 = len^2, 
                len3 = len^3)

train <- pred_dataframe1[1:1500,]
test <- pred_dataframe1[-(1:1500),]


m1 <- lm(formula= as.numeric(y)~. , data=train)
prediction1 <- predict(m1, test)

# prediction out of sample
test$p <- ifelse(prediction1>1.5,1,0)
summary(test$p ==test$y)

confusion_matrix <- table(test$p, test$y)
confusionMatrix(confusion_matrix)

hist(prediction1,
     breaks=100)


ggplot(data = pred_dataframe1, aes(x = len, y = mean_slope, col=y))+ geom_point()

pred_dataframe1 <- pred_dataframe1 %>% filter(len<2200)





# make a function that generates higher order polynomials 

if (!require(pacman)) install.packages("pacman")
p_load(ranger,
       Metrics,
       broom,
       rsample,
       tidyverse,
       stargazer,
       caret,
       glmnet)

## simple test of prediction

pred_dataframe2 <- pred_dataframe2 %>% 
  dplyr::mutate(lenriver_dist = len*river_dist,
                lenelevation_p = len*elevation_p,
                lentri_p = len*tri_p,
                lenmin_elev = len*min_elev,
                lenmean_tri = len*mean_tri)


dummies <-  model.matrix(~pred_dataframe2$cont) %>% as.data.frame()


train <- pred_dataframe1[1:2000,]
test <- pred_dataframe1[-(1:2000),]

## gjor det med lasso, sa endre hele greia, polynomials and interactions
## back out probability as a suitability measure and then standardize 
y <- train[,12]
x <- data.matrix(train[,-12])

## Estimation using lasso regression
fit <-  glmnet(x, y, family = "binomial", standardize = T)

plot(fit, xvar = "lambda", label = TRUE)

cvfit <-  cv.glmnet(x, y, family = "binomial", type.measure = "class")
lambda.min <- cvfit$lambda.min

plot(cvfit)

coef(fit, s=lambda.min)

prediction1 <- predict(fit, 
                       data.matrix(test[-12]), 
                       type = "response", 
                       s = lambda.min)

test$p <- ifelse(prediction1>0.5,1,0)
summary(test$p == test$y)

confusion_matrix <- table(test$p, test$y)
confusionMatrix(confusion_matrix)


hist(prediction1, breaks = 100)




library(ROCR)

pred1 <- prediction(prediction1, test$y)
perf1 <- performance(pred1, "tpr", "fpr")

plot(perf1, col="black")
abline(a=0, b= 1,col="grey", lty=2)




## ridge, lasso, elastic net, what variables matter? which ones are staying

# test_sample <- no_port_df %>% data.frame() %>%  dplyr::select(-y) %>%
#   rename(slope=slope_sample, aspect=aspect_sample, flowdir=flowdir_sample,
#          min_elev=min_elev_sample, max_elev=max_elev_sample) %>%
#   filter(!is.na(aspect))
# 
# # predicting in the random sample
# prediction1 <- predict(m1, data = test_sample)
# prediction1 <- prediction1[["predictions"]]
# test$p <- prediction1
# summary(test$p==test$y)
# 
# 
# model1 <- ranger(formula= as.numeric(y)~., 
#                  data=df1, 
#                  num.trees = 1000, 
#                  mtry =1)
# 
# prediction1 <- predict(model1, data=sample_df)
# prediction1 <- prediction1[["predictions"]]
# 
# 
# 
# 
# # save(pred_dataframe, file="output/pred_dataframe3.rds")
# 
# # sjekk om den ikke er sa god fordi den ikke overlapper coastline... 
# # robustness check burde brukes her
# 
# 
# 
# 
# ## testing by looking at the data
# 
# # mozambique_shape <- countries10[countries10@data$ADMIN=="Mozambique",]
# # 
# # buffer <- gBuffer(mozambique_shape, width = 10)
# # coastline_study_area <- gIntersection(buffer, coastline10) 
# # 
# 
# 
# 
# # Make a random dataset
# 
# 
# port_df1 <- port_df %>% as.data.frame() %>%filter(!is.na(slope_port)) %>% 
#   rename(slope=slope_port, tri=tri_port)
# 
# model1 <- ranger(formula= as.numeric(y)~slope, 
#                  data=port_df1, 
#                  num.trees = 1000, 
#                  mtry =1)
# 
# prediction1 <- predict(model1, data=port_df1)
# prediction1 <- prediction1[["predictions"]]
# 
# 
# 
# swe <- countries10[countries10@data$ADMIN=="Sweden",]
# tm_shape(swe)+tm_borders()+tm_shape(sample)+tm_dots(size=0.4, shape=2)+
#   tm_shape(ports)+tm_dots(size=0.4, shape=1)+
#   tm_shape(coastline10)+tm_lines() 
# 
# elev_swe <- mask(tri, swe)
# 
# tm_shape(elev_swe) + tm_raster()+tm_shape(swe)+tm_borders()


library(randomForest)

m1 <- randomForest(
  formula = y ~ .,
  data    = training_data
)

ggplot(data=pred_dataframe1, aes(x=log(len), y=river_dist,color=as.factor(y)))+geom_point(alpha=0.8)

plot(m1)


## careful walkthrough of the model
## estimating the model: plot the number of trees, tuning parameters
## validation: accuracy table, roc curves, variable importance 
## examples: these are the ports, most vs least likely port areas picture
## what kinds of ports is it able to predict? should be better at predicting 
## ports built on geographic features, 

## then causal inference with difference in difference





# ## Hyperparameter tuning
# Define the tuning grid: tuneGrid
mygrid <- data.frame(
  mtry = c(1,2,3,4,5,6,7,8,9,10)
)

# Fit random forest: model
model <- train(
  y ~ .,
  tuneGrid = mygrid,
  data = training_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE)
)


mtry <- model[["results"]][["mtry"]]
accu <- model[["results"]][["Accuracy"]]

tuning1 <- cbind(mtry, accu) %>% as.data.frame()
optimal <- order(-tuning1$accu)[1:3]



p1 <- ggplot(data=tuning1, aes(x=mtry, y=accu))+geom_point()+geom_line(linetype="dashed")+
  xlab("No. variables")+
  ylab("Accuracy") +
  ggtitle("Tuning: Number of variables")+
  geom_point(size = 1,
             shape = 21,
             fill = "black") +
  geom_vline(xintercept=optimal[1],
             color="black",
             linetype="dashed",
             size=0.3,
             alpha = 0.5)+
  geom_vline(xintercept=optimal[2],
             color="black",
             linetype="dashed",
             size=0.3,
             alpha = 0.5)+
  geom_vline(xintercept=optimal[3],
             color="black",
             linetype="dashed",
             size=0.3,
             alpha = 0.5)+
  theme(strip.background = element_blank(),
        strip.placement = "outside")+
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9))

  
  




## Model estimation
library(randomForest)

m1 <- randomForest(
  formula = y ~ .,
  data    = training_data,
  mtry=5
)

plot(m1)

n1 <- m1[["err.rate"]][,1]
n2 <- m1[["err.rate"]][,2]
n3 <- m1[["err.rate"]][,3]
tree <- c(1:500)

estimation <- cbind(n1,n2,n3,tree) %>% as.data.frame() %>% 
  gather(n, k, 1:3)

# Highest accuracy

error <- 1-accu[optimal[1]]

p2 <- ggplot(data=estimation, aes(x=tree, y=k)) + geom_line(aes(linetype=as.factor(n)))+
  geom_hline(yintercept=error,
             color="black",
             linetype="dashed",
             size=0.3,
             alpha = 0.5)+
  ggtitle("Estimation")+
  theme(strip.background = element_blank(),legend.position = "none",
        strip.placement = "outside")+
  ylab("Error rate")+
  xlab("No. trees")

ggarrange(p1,p2,ncol=2)




## Sett opp dette selv, mye bedre


training_data$y <- (training_data$y)

model <- ranger(formula= as.numeric(y)~.,
                data=training_data,
                num.trees = 1000,
                mtry = 9)
prediction <- predict(model, testing_data)$predictions-1
prediction <- ifelse(prediction>0.5,1,0)

confusion_matrix <- table(prediction, testing_data$y)
confusionMatrix(confusion_matrix)

ntrees <- c(1, 10, 25,50,100,150,200, 250, 1000, 5000)
acc <- c()
k <- 1
for (i in ntrees){

  model <- ranger(formula= as.numeric(y)~.,
                  data=training_data,
                  num.trees = i,
                  mtry = 9)
  prediction <- predict(model, testing_data)$predictions-1
  prediction <- ifelse(prediction>0.5,1,0)
  
  confusion_matrix <- table(prediction, testing_data$y)
  c <- confusionMatrix(confusion_matrix)
  acc[k] <- c[["overall"]][["Accuracy"]]
  k <- k+1

  print(i)
  
}
  
tuning2 <- cbind(ntrees=1:length(ntrees), acc) %>% as.data.frame() %>% 
  mutate(ntrees=as.factor(ntrees))

p2 <- ggplot(data=tuning2, aes(x=ntrees, y=acc, group = 1)) + 
  geom_path(linetype=2)+
  geom_point()+
  geom_hline(yintercept=0.71,
             color="black",
             linetype="dashed",
             size=0.3,
             alpha = 0.5)+
  ggtitle("Tuning: number of trees")+
  theme(strip.background = element_blank(),legend.position = "none",
        strip.placement = "outside")+
  ylab("Accuracy")+
  xlab("No. trees")+
  scale_x_discrete(labels = c("1"="1","2"="10", "3"="25","4"="50","5"="100",
                              "6"="150", "7"="200", "8"="250", "9"="1000", 
                              "10"="5000"))+
  scale_y_continuous(limits=c(0.4, 0.8))



ggarrange(p1,p2)


