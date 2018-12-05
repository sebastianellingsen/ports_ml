
################################################################
## This file splits data in test and training and fits various
## models on the data. The results are evaluated using 
## cross validation.
################################################################

###############################
## Loading data and packages ##
###############################

source("data_prep.R")

if (!require(pacman)) install.packages("pacman")
p_load(ranger, Metrics, dplyr, purr, broom, rsample)


######################
## Model evaluation ##
######################

## Splitting the data to training, testing  
data_split <- initial_split(data, 0.75)
training_data <- training(data_split)
testing_data <- testing(data_split)

## Cross validation folds
cv_split <- vfold_cv(training_data, v = 5)
cv_data <- cv_split %>% 
  mutate(train = map(splits, ~training(.x)), validate = map(splits, ~testing(.x)))

## Fitting the model
cv_models <- cv_data %>% 
  mutate(forest_model = map(train, ~ranger(formula= port~., data=data, num.trees = 100, mtry = 10)))

cv_prep <- cv_models %>% 
  mutate(validate_actual = map(validate, ~.x$port)) %>% 
  mutate(predicted = map2(forest_model, validate, ~predict(.x,.y)$predictions)) 

## Validating the fit of the model
cv_eval <- cv_prep %>% mutate(y_pred = map(cv_eval$predicted, ~ifelse(.x>0.5, 1, 0))) 

cv_eval <-  cv_eval %>% 
  mutate(y_pred = map(cv_eval$predicted, ~ifelse(.x>0.5, 1, 0))) %>% 
  mutate(error = map2_dbl(validate_actual, y_pred, ~mae(.x,.y))) %>% 
  mutate(precision = map2_dbl(validate_actual, y_pred, ~precision(.x,.y))) %>% 
  mutate(accuracy = map2_dbl(validate_actual, y_pred, ~accuracy(.x,.y)))

confusion_matrix <- map2(cv_eval$validate_actual, cv_eval$y_pred, ~table(.x,.y))
  
er <- map(cv_eval$error, ~mean(.x)) %>% unlist() %>% sort(decreasing=TRUE)
plot(er)

## Fitting the full model to the training data
model <- ranger(formula= port~., data=training_data, num.trees = 100, mtry = 10)

prediction <- predict(model, data)$predictions
y_pred <- ifelse(predict(fmodel, df)$predictions>0.3, 1, 0)
eval_df <- data.frame(y, y_pred, prediction)


###########################
## Joining the dataframe ##
###########################

hexagons <- gIntersection(hexagons, iceland, byid = TRUE)
ID <- sapply(hexagons@polygons, function(x) x@ID)
row.names(eval_df) <- ID

sps_df <- SpatialPolygonsDataFrame(hexagons, eval_df, match.ID = TRUE)


#######################
## Plotting the data ##
#######################

tm_shape(sps_df) +
  tm_fill(col="prediction", palette=plasma(256)) + tm_layout(frame=FALSE) +
  tm_shape(hexagons) +
  tm_borders(col = "white") +
  tm_shape(ports_iceland) +
  tm_dots(size=0.1) 




