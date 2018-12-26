
################################################################
## This file splits data in test and training and fits various
## models on the data. The results are evaluated using 
## cross validation.
################################################################

###############################
## Loading data and packages ##
###############################

#source("data_prep.R")

if (!require(pacman)) install.packages("pacman")
p_load(ranger, Metrics, broom, rsample, tidyverse)

######################
## Model evaluation ##
######################

#dataset = rbind(dataset_samerica, dataset_namerica, dataset_africa, dataset_asia, dataset_europe)
set.seed(11042018)
 
#save(dataset, file="dataset.Rda")
load("dataset.Rda")

# Generate final balanced dataset 
data <- dataset_samerica[which(!is.na(dataset$V2)),]
dataset_ports <- dataset %>% filter(y==1)
dataset_noports <- dataset %>% filter(y==0)
subsample_dataset_noports = sample_n(dataset_noports, size = nrow(dataset_ports))
dataset = bind_rows(subsample_dataset_noports, dataset_ports)
data$y <- as.numeric(data$y)-1

## Splitting the data to training, testing  
data_split <- initial_split(data, 0.75)
training_data <- training(data_split)
testing_data <- testing(data_split)

## Cross validation folds
cv_split <- vfold_cv(training_data, v = 10)
cv_data <- cv_split %>% 
  mutate(train = map(splits, ~training(.x)), validate = map(splits, ~testing(.x)))

## Fitting the model
cv_models <- cv_data %>% 
  mutate(forest_model = map(train, ~ranger(formula= y~., data=.x, num.trees = 2, mtry = 10)))

cv_prep <- cv_models %>% 
  mutate(validate_actual = map(validate, ~.x$y)) %>% 
  mutate(predicted = map2(forest_model, validate, ~predict(.x,.y)$predictions)) 

## Validating the fit of the model
#dta$prediction <- ifelse(pred>=0.3,1,0)
confusion_matrix <- map2(cv_prep$validate_actual, cv_prep$predicted, ~table(.x,.y))
Reduce(`+`, confusion_matrix)

cv_eval  %>% 
  mutate(error = map2_dbl(validate_actual, y_pred, ~mae(.x,.y))) %>% 
  mutate(precision = map2_dbl(validate_actual, y_pred, ~precision(.x,.y))) %>% 
  mutate(accuracy = map2_dbl(validate_actual, y_pred, ~accuracy(.x,.y)))

  
er <- map(cv_eval$error, ~mean(.x)) %>% unlist() %>% sort(decreasing=TRUE)
plot(er)

## Testing data
model_testing <- ranger(formula= y~., data=training_data, num.trees = 2, mtry = 10)
prediction <- predict(model_testing, testing_data)$predictions
confusion_matrix <-table(prediction, testing_data$y)










# ROC curves
library(ROCR)




## Fitting the full model to the training data
model <- ranger(formula= y~., data=training_data, num.trees = 30, mtry = 10)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

plot(perf)


x_rf <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf <- y_rf[x_rf<=1]
x_rf <- x_rf[x_rf<=1]



model <- ranger(formula= y~., data=training_data, num.trees = 2, mtry = 10)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

x_rf1 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf1 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf1 <- y_rf1[x_rf1<=1]
x_rf1 <- x_rf1[x_rf1<=1]



model <- ranger(formula= y~., data=training_data, num.trees = 30, mtry = 150)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

x_rf2 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf2 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf2 <- y_rf2[x_rf2<=1]
x_rf2 <- x_rf2[x_rf2<=1]


model <- ranger(formula= y~., data=training_data, num.trees = 50, mtry = 80)

prediction <- predict(model, testing_data)$predictions
pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")
x_rf3 <- perf@alpha.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf3 <- perf@y.values %>% unlist() %>% sort(decreasing=TRUE) 
y_rf3 <- y_rf3[x_rf3<=1]
x_rf3 <- x_rf3[x_rf3<=1]



ggplot() + 
  geom_line(aes(x_rf, y_rf), color="Blue") + 
  #geom_line(aes(x_rf1, y_rf1), color="Red") +
  #geom_line(aes(x_rf2, y_rf2), color="Red") +
  #geom_line(aes(x_rf3, y_rf3), color="Orange") +
  xlab("False positive rate") + ylab("True positive rate") + ggtitle("Receiver operating characteristic") +
  geom_segment(aes(x = 0, y = 0, xend = 0.98, yend = 1, colour = "segment"),alpha=0.3, linetype = 2) +
  scale_x_continuous(limits = c(0, 1)) + theme_classic() 
                     



# shows the roc of a pruned random forest





###########################
## Joining the dataframe ##
###########################

hexagons_full <- gIntersection(hexagons, iceland, byid = TRUE)

ID <- sapply(hexagons_full@polygons, function(x) x@ID)
row.names(eval_df) <- ID

sps_df <- SpatialPolygonsDataFrame(hexagons_full, eval_df, match.ID = TRUE)


#######################
## Plotting the data ##
#######################

tm_shape(sps_df) +
  tm_fill(col="prediction", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons_full) + tm_borders(col = "white") +
  tm_shape(ports_iceland) + tm_dots(size=0.1) 


hexagons <- gIntersection(hexagons, study_area, byid = TRUE)
row.names(dataset) <- paste(row.names(dataset), " 188", sep="")
cut_data <- dataset[row.names(dataset)  %in%  sapply(hexagons@polygons, function(x) x@ID),]
sps_df <- SpatialPolygonsDataFrame(hexagons, cut_data, match.ID = TRUE)

tm_shape(sps_df) +
  tm_fill(col="y") + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") 

+
  tm_shape(ports_iceland) +
  tm_dots(shape = 1, size=0.1)  




## To do:
## combine the predicted port data with the aggregated raster







