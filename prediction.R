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
p_load(ranger, Metrics, broom, rsample, tidyverse, stargazer, caret)


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
dataset <- data_pred %>%
  mutate_all(type.convert) %>%
  mutate_if(is.factor, as.numeric) 

dataset[is.na(dataset)] <- 0
dataset$y <- as.factor(dataset$y)

dataset_ports <- dataset %>% filter(y==1)
dataset_noports <- dataset %>% filter(y==0)
subsample_dataset_noports = sample_n(dataset_noports, size = nrow(dataset_ports))
dataset = bind_rows(subsample_dataset_noports, dataset_ports)
dataset <- dataset[sample(nrow(dataset)),]


#dataset <- dataset %>% select(-(V3:V8), -(V287:V290))

#data$y <- as.numeric(data$y)-1
#dataset %>% mutate(y=as.numeric(y)) %>% dplyr::select(y) %>%  as.data.frame %>% stargazer(type="text")

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
  mutate(forest_model = map(train, ~ranger(formula= y~., data=.x, num.trees = 100, mtry = 20)))

cv_prep <- cv_models %>% 
  mutate(validate_actual = map(validate, ~.x$y)) %>% 
  mutate(predicted = map2(forest_model, validate, ~predict(.x,.y)$predictions)) 

## Validating the fit of the model
#dta$prediction <- ifelse(pred>=0.3,1,0)
confusion_matrix <- map2(cv_prep$validate_actual, cv_prep$predicted, ~table(.x,.y))
c_matrix <- Reduce(`+`, confusion_matrix) %>% data.frame
colnames(c_matrix) <- c("Predicted", "Actual", "Cases") 
c_matrix %>% stargazer(type="text", summary=FALSE, rownames = FALSE)

cv_eval  %>% 
  mutate(error = map2_dbl(validate_actual, y_pred, ~mae(.x,.y))) %>% 
  mutate(precision = map2_dbl(validate_actual, y_pred, ~precision(.x,.y))) %>% 
  mutate(accuracy = map2_dbl(validate_actual, y_pred, ~accuracy(.x,.y)))

er <- map(cv_eval$error, ~mean(.x)) %>% unlist() %>% sort(decreasing=TRUE)
plot(er)

## Testing data
model_testing <- ranger(formula= y~., data=training_data, num.trees = 100, mtry = 20)
prediction <- predict(model_testing, testing_data)$predictions
confusion_matrix <-table(prediction, testing_data$y)
confusionMatrix(confusion_matrix)

###############################
## Assessing on testing data ##
###############################

## ROC curves
library(ROCR)

training_data$y <- as.numeric(training_data$y)

## Fitting the full model to the training data
model1 <- ranger(formula= y~., data=training_data, num.trees = 100, mtry = 5)
model2 <- ranger(formula= y~., data=training_data, num.trees = 100, mtry = 50)
model3 <- ranger(formula= y~., data=training_data, num.trees = 100, mtry = 150)

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


## The confusion matrix
training_data$y <- (training_data$y)

model <- ranger(formula= as.factor(y)~., data=training_data, num.trees = 1000, mtry = 5)
prediction <- predict(model, testing_data)$predictions

confusion_matrix <- table(prediction, testing_data$y)
confusionMatrix(confusion_matrix) 


## Hyperparameter tuning
# Define the tuning grid: tuneGrid
mygrid <- data.frame(
  mtry = c(40,50)
)

# Fit random forest: model
model <- train(
  y ~ .,
  tuneGrid = mygrid,
  data = training_data, 
  method = "rf",
  trControl = trainControl(method = "cv", number = 3, verboseIter = TRUE)
)

# Print model to console
model

# Plot model
plot(model)


# add tuning section to the appendix











