#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

library(caret)
library(klaR)

# With Pokémon [1]
appearsDF <- as.data.frame(appearsProcessed)
# Transform pokemonId to a boolean variable
appearsDF$isPokemon<- apply(appearsDF, 1, function(appear){
  if (appear["pokemonId"] == 1) "Yes"
  else "No"
})
appearsDF$isPokemon<- as.factor(appearsDF$isPokemon)
appearsDF$pokemonId <- NULL

# Provisional
appearsDF[,coocMatches] <- NULL

# Split data 80% for training
totalRows=nrow(appearsDF)
split=0.80*totalRows

# Split dataset
dataTrain <- appearsDF[1:split,]
dataTest <- appearsDF[split:totalRows,]
trainIndex <- createDataPartition(appearsDF$isPokemon, p=split, list=FALSE)
# make predictions
xTest <- dataTest[,-13]
yTest <- dataTest[,13]
# Generate the model
model <- glm(isPokemon ~ ., family=binomial(link='logit'), data=dataTrain)

# Analyze the table of deviance
anova(model, test="Chisq")

# Assessing the predictive ability of the model
fitted.results <- predict(model,newdata=dataTest,type='response')
fitted.results <- ifelse(fitted.results > 0.2,1,0)

misClasificError <- mean(fitted.results != dataTest)
print(paste('Accuracy',1-misClasificError))

# Get the ROC curve and the AUC
library(ROCR)
p <- predict(model, newdata=dataTest, type="response")
pr <- prediction(p, dataTest$isPokemon)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

