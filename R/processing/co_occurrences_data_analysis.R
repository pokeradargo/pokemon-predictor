
## Get coo ocurrence columns
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))

# Transform co occurences to a numeric values

# Delete coo ocurrence columns and put into another variable
appearedCategories <- appearsProcessed
appearedCategories[, coocMatches] <- NULL

# Delete repeated values
appearedCategories <- unique(appearedCategories)

# Get all the levels of every category (included PokémonID)
#appearedLevels <- sapply(appearedCategories, levels)

## Put all the categoric variables on the same level
#appearedCategories$pokemonId <- factor(appearedCategories$pokemonId, levels=appearedLevels$pokemonId)

appearedCategories[1:10,]
by(appearedCategories[1:10,], 1:10, function(row, appears){
  
  appearsMatched <- appearsProcessed[which(
        appearsProcessed$pokemonId == as.character(row["pokemonId"])&
        appearsProcessed$appearedTimeOfDay == as.character(row["appearedTimeOfDay"]) &
        appearsProcessed$appearedDayOfWeek == as.character(row["appearedDayOfWeek"]) &
        appearsProcessed$terrainType == as.character(row["terrainType"]) &
        appearsProcessed$closeToWater == as.character(row["closeToWater"]) &
        appearsProcessed$continent == as.character(row["continent"]) &
        appearsProcessed$temperature == as.character(row["temperature"]) & 
        appearsProcessed$windSpeed == as.character(row["windSpeed"]) & 
        appearsProcessed$pressure == as.character(row["pressure"]) &
        appearsProcessed$weatherIcon == as.character(row["weatherIcon"])
    ),]
  (length(appearsMatched))
  
}, appears = appearsProcessed)

library(doBy)

doSomething <- function(group) {
  (group)
}
appearsGrouped <- summaryBy(. ~ pokestopDistance, data=appearsProcessed, FUN="length")

# foreach appeareance we have to count the number of co-occurences of every Pokémon
appearsProcessed <- apply(appearsProcessed, 1, countNumberOfOcurrences)