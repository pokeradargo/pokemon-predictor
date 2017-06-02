source("R/functions.R")
## Load the dataset
appears <- read.csv("data/300k.csv",header=T, check.names = FALSE)
## Display the columns
appearColNames <- colnames(appears)
## Define the number of rows to analyze
nRows <- 10000
## Store 10000 random observations into another variable to start the pre-processing
#appearsProcessed <- appears[sample(1:nRows, replace = TRUE),]
appearsProcessed <- appears

## I don't know the reference of the X_id identifier, so we can delete it.
appearsProcessed$X_id <- NULL
appearsProcessed$'_id' <- NULL

## Class represents the same than pokemonId
appearsProcessed$class <- NULL
# We have to factorize Pokémon ID
appearsProcessed$pokemonId <- as.factor(appearsProcessed$pokemonId)

# Let's analyze the time data!
source("R/processing/time_data_analysis.R")

# Let's analyze the co ocurrences data!
source("R/processing/location_data_analysis.R")

# Let's analyze the weather data!
source("R/processing/weather_data_analysis.R")

# Let's analyze the co-occurence data
source("R/processing/co_occurences_data_analysis.R")

# Generate the Naive Bayes predictor
source("R/model/naive_bayes.R")

# Generate the multinomial regression model
source("R/model/multinomial_regression.R")

