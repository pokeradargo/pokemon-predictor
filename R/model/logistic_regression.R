#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

library(ROCR)
# Let's analyze the response variable to see the most frequently Pokémon
appearsDF <- as.data.frame(read.csv("output/cooc_appears_processed.csv",header=T, sep=",", check.names = FALSE))
summary(as.factor(appearsDF$pokemonId))
# We gonna take a look to the Pokémons with more than 200 appears
pokemonIds <- c(16,19,13,21,133,96,41,10,48,129)
# Creamos varios modelos por los Pokémon con más frecuencia

for (pokemonId in pokemonIds) {
  appearsForModel <- appearsDF
  model <- createModel(appearsForModel, pokemonId)
  
  if (validateModel(model, pokemonId) == TRUE) {
    saveRDS(model, paste("model_pokemon_",toString(pokemonId),".rds",sep=""))
    print (paste("Pokémon",toString(pokemonId),"saved!", sep=" "))
  }
}




# Predicción global: predecimos cada row con los modelos que hemos creado, y acumulamos los modelos predictivos hasta llegar al 70%
# Si no llegamos al 70%, no predecimos y hacemos un "skip"


