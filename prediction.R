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
p_load(ranger, Metrics, broom, rsample, tidyverse, stargazer)


######################
## Model evaluation ##
######################

set.seed(11042018)
 
# Generate final balanced dataset 
dataset <- data %>%
  mutate_all(type.convert) %>%
  mutate_if(is.factor, as.character) 

dataset[is.na(dataset)] <- 0
dataset$y <- as.factor(dataset$y)

## Generate data for prediction
dataset_ports <- dataset %>% filter(y==1)
dataset_noports <- dataset %>% filter(y==0)
subsample_dataset_noports = sample_n(dataset_noports, size = nrow(dataset_ports))
dataset = bind_rows(subsample_dataset_noports, dataset_ports)
#data$y <- as.numeric(data$y)-1
dataset %>% mutate(y=as.numeric(y)) %>% select(y) %>%  as.data.frame %>% stargazer(type="text")

## Splitting the data to training, testing  
data_split <- initial_split(dataset, 0.75)
training_data <- training(data_split)
testing_data <- testing(data_split)

## Cross validation folds
cv_split <- vfold_cv(training_data, v = 5)
cv_data <- cv_split %>% 
  mutate(train = map(splits, ~training(.x)), validate = map(splits, ~testing(.x)))

## Fitting the model
cv_models <- cv_data %>% 
  mutate(forest_model = map(train, ~ranger(formula= y~., data=.x, num.trees = 1000, mtry = 250)))

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
model_testing <- ranger(formula= y~., data=training_data, num.trees = 50, mtry = 10)
prediction <- predict(model_testing, testing_data)$predictions
confusion_matrix <-table(prediction, testing_data$y)


###############################
## Assessing on testing data ##
###############################

## ROC curves
library(ROCR)

training_data$y <- as.numeric(training_data$y)

## Fitting the full model to the training data
model <- ranger(formula= y~., data=training_data, num.trees = 1000, mtry = 250)

prediction <- predict(model, testing_data)$predictions

pred <- prediction(prediction, testing_data$y)
perf <- performance(pred, "tpr", "fpr")

plot(perf)

## The confusion matrix
training_data$y <- as.factor(training_data$y)

model <- ranger(formula= y~., data=training_data, num.trees = 1000, mtry = 250)
prediction <- predict(model, testing_data)$predictions

confusion_matrix <- table(prediction, testing_data$y)





