#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

library(ROCR)
# Let's analyze the response variable to see the most frequently Pokémon
appearsDF <- as.data.frame(read.csv("output/cooc_appears_processed.csv",header=T, sep=" ", check.names = FALSE))
summary(as.factor(appearsDF$pokemonId))
# We gonna take a look to the Pokémons with more than 200 appears
pokemonIds <- c(13,21,133,96,41,10,48,129, 46, 98, 23, 32, 54, 29, 60, 118, 35, 43, 129, 46, 98, 23, 32, 54, 29, 60, 118, 25, 43, 120, 69, 17)
# Creamos varios modelos por los Pokémon con más frecuencia

for (pokemonId in pokemonIds) {
  appearsForModel <- appearsDF
  
  # Delete day of week
  remove(appearsForModel$appearedDayOfWeek)
  
  model <- createModel(appearsForModel, pokemonId)
  saveRDS(model, paste("../model_pokemon_",toString(pokemonId),".rds",sep=""))
  
  if (validateModel(appearsForModel, model, pokemonId) == TRUE) {
    print (paste("Pokémon",toString(pokemonId),"saved!", sep=" "))
  }
}

# Predicción global: predecimos cada row con los modelos que hemos creado, y acumulamos los modelos predictivos hasta llegar al 70%
# Si no llegamos al 70%, no predecimos y hacemos un "skip"

# Measure of creation model execution time
# ptm <- proc.time()
# model <- createModel(appearsDF, 16)
# (proc.time() - ptm)
