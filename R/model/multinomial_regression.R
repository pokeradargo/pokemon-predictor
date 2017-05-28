#http://www.jameskeirstead.ca/blog/how-to-multinomial-regression-models-in-r/
#load the library
library(nnet)

#Transform into dataframe
appearsDF <- as.data.frame(appearsProcessed)

########################################
#### Data Split ########################
########################################
split=0.80
trainIndex <- createDataPartition(appearsDF$pokemonId, p=split, list=FALSE)
data_train <- appearsDF[trainIndex,]
data_test <- appearsDF[-trainIndex,]
# Generate the model
model <- multinom(pokemonId ~ ., appearsDF, MaxNWts = 20000)
# make predictions
predictions <- predict(model, "probs")
# summarize results