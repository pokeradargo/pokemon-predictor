appearsDF <- as.data.frame(read.csv("../cooc_appears_processed.csv",header=T, sep=",", check.names = FALSE))
## Display the columns
appearColNames <- colnames(appearsDF)
## Get coo ocurrence columns
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))
# Delete coo ocurrence columns and put into another variable
appearsDF[, coocMatches] <- NULL
# Delete Pokémon ID
appearsDF$pokemonId <- NULL

# Get all the levels of every category (included PokémonID)
appearedLevels <- sapply(appearsDF, levels)

# Create a table of all possible combinations
appearedCombinations <- expand.grid(appearedLevels)
saveRDS(appearedCombinations, "../appearedCombinations.rds")

################################################################
# Load Appeared Combinations from RDS
################################################################

appearedCombinations <- readRDS("../appearedCombinations.rds")
