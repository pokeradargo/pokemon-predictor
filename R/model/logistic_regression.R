#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

library(caret)
library(klaR)
# Analizamos la variable respuesta para ver qué Pokémon aparecen más veces

# With Pokémon [10]
appearsDF <- as.data.frame(appearsProcessed)
# Transform pokemonId to a boolean variable
appearsDF$isPokemon<- apply(appearsDF, 1, function(appear){
  if (appear["pokemonId"] == 35) "Yes"
  else "No"
})
appearsDF$isPokemon<- as.factor(appearsDF$isPokemon)
appearsDF$pokemonId <- NULL

# Provisional
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))
appearsDF[,coocMatches] <- NULL

# Split data 80% for training
totalRows=nrow(appearsDF)
split=0.80*totalRows

# Split dataset
dataTrain <- appearsDF[1:split,]
dataTest <- appearsDF[split:totalRows,]

dataTest <- dataTest[which(dataTest$appearedDayOfWeek != "viernes"),]

#trainIndex <- createDataPartition(appearsDF$isPokemon, p=split, list=FALSE)
# Generate the model
model <- glm(isPokemon ~ ., family=binomial(link='logit'), data=dataTrain)

# Analyze the table of deviance
anova(model, test="Chisq")

# Assessing the predictive ability of the model
fitted.results <- predict(model,newdata=dataTest,type='response')
fitted.results <- ifelse(fitted.results > 0.18,1,0)

# Analizamos la variable T de Student para validar si cada modelo es correcto
t.test(fitted.results  ~ dataTest$isPokemon)
boxplot(fitted.results  ~ dataTest$isPokemon)

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

# Creamos varios modelos por los Pokémon con más frecuencia

# Predicción global: predecimos cada row con los modelos que hemos creado, y acumulamos los modelos predictivos hasta llegar al 70%
# Si no llegamos al 70%, no predecimos y hacemos un "skip"


