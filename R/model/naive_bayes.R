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
# train a naive bayes model
model <- NaiveBayes(pokemonId~., data=data_train)
# make predictions
x_test <- data_test[,-1]
y_test <- data_test[,1]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)

# Accuracy -> 0.162

##########################################
##### Bootstrap Resampling ###############
##########################################

# define training control with 100 iterations
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(pokemonId~., data=appearsDF, trControl=train_control, method="nb")
# summarize results
print(model)

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

###################################################
## Repeated k-fold Cross Validation ###############
###################################################

# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(pokemonId~., data=appearsDF, trControl=train_control, method="nb")
# summarize results
print(model)

##################################################
## Leave One Out Cross Validation ################
##################################################

# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(pokemonId~., data=appearsDF, trControl=train_control, method="nb")
# summarize results
print(model)


