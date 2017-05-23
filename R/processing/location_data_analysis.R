####################### LOCATION DATA #################################################
## The information about the location where Pokémon appeared
###################################################################################
### "rural","midurban","suburban","urban" (boolean variable) -> defines the population density
### latitude, longitude (continous variable) -> included in appeared Local time
### gymInXm (boolean variable) -> defines if the distance from a gym is greater than a value
### gymDistanceKm (continous variable) -> defines the distance from a gym
### pokestopInX (boolean variable) -> defines if the distance from a pokestop is greater than a value
### pokestopDistanceKm (boolean variable) -> defines the distance from a pokestop
### closeToWater (boolean variable) - defines if the position it's close to the Water
### population_density (continous variable) - what is the population density per square km
## TerrainType (continous type) - http://glcf.umd.edu/data/lc/
###################################################################################

## Join (urban, suburban, midurban and rural) to only one factor variable
appearsProcessed[1:100,c("urban","suburban","midurban","rural")] # Not sure to transform it, because a row can be multiple values
unique(appearsProcessed[,c("urban","suburban","midurban","rural")]) # Check the posible combinations
urbanColNames <- c("rural","midurban","suburban","urban")
appearsProcessed$urbanization <- apply(appearsProcessed[,urbanColNames], 1, defineUrban)
appearsProcessed$urbanization <- as.factor(appearsProcessed$urbanization)
appearsProcessed[,urbanColNames] <- NULL
## This variable represents the factorization of population_density, so population_density can be excluded
## from our dataset because we are not interested in the specific number of population
appearsProcessed$population_density <- NULL

## We can transform the gymDistance into a factor variable
gymInMatches <- subset(appearColNames, grepl("gymIn", appearColNames))
## Delete Gym distance numeric variable
appearsProcessed$gymDistanceKm <- NULL
appearsProcessed$gymDistance <- apply(appearsProcessed[, gymInMatches],1, tranformGymDistanceToFactorVariable)
appearsProcessed$gymDistance <- as.factor(appearsProcessed$gymDistance)
appearsProcessed[, gymInMatches] <- NULL

## We can transform the PokéStop distance into a factor variable
pokestopInMatches <- subset(appearColNames, grepl("pokestopIn", appearColNames))
appearsProcessed$pokestopDistance <- apply(appearsProcessed[, pokestopInMatches], 1, tranformPokeStopDistanceToFactorVariable)
appearsProcessed$pokestopDistance <- as.factor(appearsProcessed$pokestopDistance)
## Delete Pokéstop distance numeric variable
appearsProcessed$pokestopDistanceKm <- NULL
appearsProcessed[, pokestopInMatches] <- NULL

### Transform terrain Type variable to understanding factor variable
appearsProcessed$terrainType <- apply(appearsProcessed, 1, transformTerrainType)
appearsProcessed$terrainType <- as.factor(appearsProcessed$terrainType)

## The location coordinates are not relevant for this study
locationCoordinatesMatches <- subset(appearColNames, grepl("cellId|latitude|longitude", appearColNames))
appearsProcessed[, locationCoordinatesMatches] <- NULL

## Transform closeToWater variable into categories
appearsProcessed$closeToWater <- apply(appearsProcessed, 1, tranformCloseToWaterToFactorVariable)
appearsProcessed$closeToWater <- as.factor(appearsProcessed$closeToWater)