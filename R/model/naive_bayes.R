# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# load the libraries
library(caret)
library(klaR)

########################################
#### Data Split ########################
########################################

appearsDF <- as.data.frame(appearsProcessed)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(appearsDF$pokemonId, p=split, list=FALSE)
data_train <- appearsDF[trainIndex,]
data_test <- appearsDF[-trainIndex,]
# make predictions
x_test <- data_test[,-1]
y_test <- data_test[,1]
# train a naive bayes model

## Choose a FactorLaplace range
## -2.0 => 2.0
NbHyperParameters <- seq(-2.0,2.0, by = 0.2)
maxAccuracy <- 0
bestParameter <- NULL
for(parameter in NbHyperParameters) {
  model <- NaiveBayes(pokemonId~., data=data_train, fl=parameter)
  predictions <- predict(model, x_test)
  # summarize results
  cm <- confusionMatrix(predictions$class, y_test)
  
  if (is.null(bestParameter)) {
    maxAccuracy <- cm$overall['Accuracy']
    bestParameter <- parameter
  } else if (maxAccuracy < cm$overall['Accuracy']) {
    maxAccuracy <- cm$overall['Accuracy']
    bestParameter <- parameter
  }
}

###############################
# Overall Statistics
# Accuracy : 0.1604          
# 95% CI : (0.1575, 0.1634)
# No Information Rate : 0.1762          
# P-Value [Acc > NIR] : 1               
# Kappa : 0.027          
# Mcnemar's Test P-Value : NA   
##############################

##########################################
##### Bootstrap Resampling ###############
##########################################

# define training control with 100 iterations
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(pokemonId~., data=appearsDF, trControl=train_control, method="nb")
# summarize results
print(model)
predictions <- predict(model, x_test)
confusionMatrix(predictions$class, y_test)

##########################################
## k-fold Cross Validation ###############
##########################################

# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(pokemonId~., data=appearsDF, trControl=train_control, method="nb", tuneGrid=grid)
# summarize results
print(model)
predictions <- predict(model, x_test)
confusionMatrix(predictions$class, y_test)

###############################
# Overall Statistics
# Accuracy : 0.1651          
# 95% CI : (0.1621, 0.1681)
# No Information Rate : 0.1762          
# P-Value [Acc > NIR] : 1               
# Kappa : 0.0322          
# Mcnemar's Test P-Value : NA   
##############################


###################################################
## Repeated k-fold Cross Validation ###############
###################################################

# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(pokemonId~., data=appearsDF, trControl=train_control, method="nb")
# summarize results
print(model)
predictions <- predict(model, x_test)
confusionMatrix(predictions$class, y_test)

###############################
# Overall Statistics
# Accuracy : 0.1651          
# 95% CI : (0.1621, 0.1681)
# No Information Rate : 0.1762          
# P-Value [Acc > NIR] : 1               
# Kappa : 0.0322          
# Mcnemar's Test P-Value : NA   
##############################

##################################################
## Leave One Out Cross Validation ################
##################################################

# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(pokemonId~., data=appearsDF, trControl=train_control, method="nb")
# summarize results
print(model)
predictions <- predict(model, x_test)
confusionMatrix(predictions$class, y_test)

