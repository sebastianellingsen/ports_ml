## This script calculates and prints the confusion matrix

## The confusion matrix with rf
training_data$y <- (training_data$y)

model <- ranger(formula= as.numeric(y)~., 
                data=training_data, 
                num.trees = 5000,
                mtry = 1)
prediction <- predict(model, testing_data)$predictions-1
prediction <- ifelse(prediction>0.5,1,0)

confusion_matrix <- table(prediction, testing_data$y)
cmatrix1 <- confusionMatrix(confusion_matrix) 

## The confusion matrix with ols
training_data$y <- (training_data$y)

model <- lm(formula= as.numeric(y)~., data=training_data)
m_fit <- predict(model, testing_data)-1
prediction <- ifelse(m_fit>0.5,1,0)

confusion_matrix <- table(prediction, testing_data$y)
cmatrix2 <- confusionMatrix(confusion_matrix) 

