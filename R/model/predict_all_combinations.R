install.packages("elasticsearchr")
library(elasticsearchr)

getModelOutput <- function(row) {
  minProbability <- 0.7
  maxPokemons <- 5
  pokemonIds <- c(10,13,16,17,19,21,23,25,29,32,35,41,43,46,48,54,60,69,96,98,118,120,129,133)
  prediction <- rep(0, length(pokemonIds))
  results <- data.frame("pokemonId" = pokemonIds, prediction)

  for (pokemonId in pokemonIds) {
    model <- readRDS(paste('../model_pokemon_', toString(pokemonId),'.rds',sep=""))
    fittedResult <- predict(model, newdata=row,type='response')
    
    results[results$pokemonId == pokemonId,]$prediction <- fittedResult
    
    remove(model)
  }
  
  resultsOfPrediction <- results
  
  predictedPokemons <- list()
  accumulatedPrediction <- 0
  
  while(accumulatedPrediction < minProbability && length(predictedPokemons) < maxPokemons) {
    
    maxPredictionIndex <- which.max(resultsOfPrediction$prediction)
    bestPrediction <- resultsOfPrediction[maxPredictionIndex,]
    resultsOfPrediction <- resultsOfPrediction[-maxPredictionIndex,]
    
    predictedPokemons <- append(predictedPokemons, list(bestPrediction$pokemonId))
    accumulatedPrediction <- accumulatedPrediction + bestPrediction$prediction
  }
  
  modelOutput <- data.frame(predictedPokemons=paste(unlist(predictedPokemons), collapse = ","), accumulatedPrediction)
  
  modelOutput
}

appearsDF <- as.data.frame(read.csv("../cooc_appears_processed.csv",header=T, sep=" ", check.names = FALSE))
## Display the columns
appearColNames <- colnames(appearsDF)
## Get coo ocurrence columns
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))
remove(appearsDF)

appearedCombinations <- readRDS("../appearedCombinations.rds")

for (i in seq(1, length(appearedCombinations))) {
  appearedCombination <- appearedCombinations[i,]
  # Set coo ocurrence columns to 0
  appearedCombination[, coocMatches] <- 0
  # Get model output
  modelOutput <- getModelOutput(appearedCombination)
  appearedCombination[, coocMatches] <- NULL
  output <- cbind(appearedCombination, modelOutput)
  es <- elastic("http://ci.adsmurai.net:9200", "dataoutput", "json") %index% output
}

