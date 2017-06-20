appearsCategories <- appearsDF
## Get coo ocurrence columns
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))
# Delete coo ocurrence columns and put into another variable
appearedCategories <- appearsProcessed
appearedCategories[, coocMatches] <- NULL
# Delete Pokémon ID
appearedCategories$pokemonId <- NULL

# Get all the levels of every category (included PokémonID)
appearedLevels <- sapply(appearedCategories, levels)

# Create a table of all possible combinations
appearedCombinations <- expand.grid(appearedLevels)