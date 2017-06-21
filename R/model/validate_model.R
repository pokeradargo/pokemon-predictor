appearsDF <- as.data.frame(read.csv("../cooc_appears_processed.csv",header=T, sep=",", check.names = FALSE))

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
  
  results[results$pokemonId == 16,]$prediction <- fittedResult
    
  remove(model)
}

bestPrediction <- results[which.max(results$prediction),]

if (pokemonRealId == bestPrediction$pokemonId) {
  "That's great!"
}