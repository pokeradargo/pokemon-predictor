source("R/functions.R")
## Load the dataset
appears <- read.csv("data/300k.csv",header=T, check.names = FALSE)
## Display the columns
appearColNames <- colnames(appears)
appearsProcessed <- appears

## Delete co-occurrence variables
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))
appearsProcessed[, coocMatches] <- lapply(appearsProcessed[, coocMatches], as.numeric)

## I don't know the reference of the X_id identifier, so we can delete it.
appearsProcessed$X_id <- NULL
appearsProcessed$'_id' <- NULL

## Pokemon ID represents the same than class...
appearsProcessed$pokemonId <- NULL

# Let's analyze the time data!
source("R/processing/time_data_analysis.R")

# Let's analyze the location data!
source("R/processing/location_data_analysis.R")

# Let's analyze the wather data!
source("R/processing/weather_data_analysis.R")