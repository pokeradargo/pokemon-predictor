appearsDF <- as.data.frame(read.csv("../cooc_appears_processed.csv",header=T, sep=",", check.names = FALSE))


minProbability <- 0.7
maxPokemons <- 5

pokemonId <- c(10,13,16,17,19,21,23,25,29,32,35,41,43,46,48,54,60,69,96,98,118,120,129,133)
prediction <- rep(0, length(pokemonIds))
results <- data.frame(pokemonId, prediction)

row <- appearsDF[1,]
pokemonRealId <- row$pokemonId
row$pokemonId <- NULL
row$appearedDayOfWeek <- 'jueves'

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

if (accumulatedPrediction < minProbability) {
  FALSE
} else {
  if (is.null(predictedPokemons[[pokemonRealId]])) {
    FALSE
  } else {
    TRUE
  }
}

