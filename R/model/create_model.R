
isPokemon<- function(appear, pokemonId) {
  if (as.numeric(appear["pokemonId"]) == pokemonId) "Yes"
  else "No"
} 

createModel <- function(appears, pokemonId) {
  # Transform pokemonId to a boolean variable
  appears$isPokemon<- apply(appears, 1, isPokemon, pokemonId=pokemonId)
  
  appears$isPokemon<- as.factor(appears$isPokemon)
  appears$pokemonId <- NULL
  
  "Hola Buenos DIAS"
  
  # Split data 80% for training
  totalRows=nrow(appears)
  split=0.80*totalRows
  
  # Split dataset
  dataTrain <- appears[1:split,]
  dataTest <- appears[split:totalRows,]

  # Generate the model
  model <- glm(isPokemon ~ ., family=binomial(link='logit'), data=dataTrain)
  
  model
}


validateModel <- function(model, pokemonId) {
  # Analyze the table of deviance
  #anova(model, test="Chisq")
  
  # Assessing the predictive ability of the model
  fitted.results <- predict(model,newdata=dataTest,type='response')
  fitted.results <- ifelse(fitted.results > 0.4,1,0)
  
  # Analizamos la variable T de Student para validar si cada modelo es correcto
  tStudent <- t.test(fitted.results  ~ dataTest$isPokemon)
  #boxplot(fitted.results  ~ dataTest$isPokemon)
  
  misClasificError <- mean(fitted.results != dataTest)
  #print(paste('Accuracy',1-misClasificError))
  
  # Get the ROC curve and the AUC
  p <- predict(model, newdata=dataTest, type="response")
  pr <- prediction(p, dataTest$isPokemon)
  #prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  #plot(prf)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  
  isAGoodModel(tStudent, misClasificError, auc)
}

isAGoodModel<- function(tStudent, misClasificError, auc) {
  if (
    tStudent$statistic < -1 &&
    tStudent$p.value < 0.05 && 
    misClasificError < 0.3 && 
    auc > 0.5
  ) {
    TRUE
  } else {
    FALSE
  }
}