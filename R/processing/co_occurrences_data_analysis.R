
## Get coo ocurrence columns
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))

# Delete coo ocurrence columns and put into another variable
appearedCategories <- appearsProcessed
appearedCategories[, coocMatches] <- NULL

# Get all the levels of every category (included PokémonID)
appearedLevels <- sapply(appearedCategories, levels)

# Create a table of all possible combinations
# appearedCombinations <- expand.grid(appearedLevels) So hard...

library(doBy)

doSomething <- function(group) {
  (group)
}
appearsGrouped <- summaryBy(. ~ pokestopDistance, data=appearsProcessed, FUN="length")

# foreach appeareance we have to count the number of co-occurences of every Pokémon
appearsProcessed <- apply(appearsProcessed, 1, countNumberOfOcurrences)