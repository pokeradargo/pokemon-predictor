####################### WEATHER DATA ##############################################
## The information about the weather at the appearing moment of a Pokémon
###################################################################################
### weather (factor variable) -> provides the general information about the weather
### temperature (continous variable) -> temperature in celsius
### windSpeed (continous variable) -> speed of the wind in km/h at the location 
### windBearing (continous variable) -> wind direction
### pressure (continous variable) ->  atmospheric pressure in bar at the location
### weatherIcon (continous variable) ->   compact representation of the weather at the location
### sunriseMinutesMidnight-sunsetMinutesBefore (continous variable) -> time of appearance relatively to sunrise/sunset (splitted by hours and minutes)
###################################################################################

## Check the difference of weather and weatherIcon
unique(appearsProcessed$weather)
unique(appearsProcessed$weatherIcon)
### weather has 26 levels, it's a lot and weather-icon it's compacted. So we can ignored from our study
appearsProcessed$weather <- NULL

## Delete the sunset and sunrise information because you can only compare this variables with appearings at the same time region
## and the problem gets a complexity that we want to avoid
sunsetTimeCols <- subset(appearColNames, grepl("sunsetMinute|sunsetHour", appearColNames))
appearsProcessed[, sunsetTimeCols] <- NULL

## Delete the sunset and sunrise information because you can only compare this variables with appearings at the same time region
## and the problem gets a complexity that we want to avoid
sunriseTimeCols <- subset(appearColNames, grepl("sunriseHour|sunriseMinute", appearColNames))
appearsProcessed[, sunriseTimeCols] <- NULL

## Transform temperature as factor variable
## Take a look of the data
summary(appears$temperature)
hist(appears$temperature)
boxplot(appears$temperature)
################################################
# Very Cold < 0
# Cold 0 - 10
# Tempered 10 - 20
# Warm 20 - 30
# Very Warm > 30
################################################
appearsProcessed$temperature <- apply(appearsProcessed, 1, transformTemperatureToFactorVariable)
appearsProcessed$temperature <- as.factor(appearsProcessed$temperature)

## Transform atmospheric pressure as factor variable
## Take a look of the data
summary(appears$pressures)
hist(appears$pressure)
boxplot(appears$pressure)
################################################
# Low < 1005
# Normal < 1018
# High > 1018
################################################
appearsProcessed$pressure <- apply(appearsProcessed, 1, transformAtmosphericPressureToFactorVariable)
appearsProcessed$pressure <- as.factor(appearsProcessed$pressure)

## Transform wind speed as factor variable
## Take a look of the data
summary(appears$windSpeed)
hist(appears$windSpeed)
boxplot(appears$windSpeed)
# https://en.wikipedia.org/wiki/Beaufort_scale
################################################
# Calm < 5
# Light Air <= 6
# Light breeze <= 12
# Gentle breeze <= 20
# Moderate breeze <= 29
# Fresh breeze <= 38
# Strong breeze <= 50
# High wind > 50
################################################
appearsProcessed$windSpeed <- apply(appearsProcessed, 1, tranformWindSpeedToFactorVariable)
appearsProcessed$windSpeed <- as.factor(appearsProcessed$windSpeed)

## Transform wind bearing as factor variable
## Take a look of the data
summary(appears$windBearing)
hist(appears$windBearing)
boxplot(appears$windBearing)
#http://www.physicalgeography.net/fundamentals/7n.html
################################################
# North 337.5 - 360, 0 - 22.5 
# North-East 22.5 - 67.5
# East 67.5 - 112.5
# South East 112.5 - 157.5
# South <= 157.5 - 202.5
# South West <= 202.5 - 247.5
# West <= 247.5 - 292.5
# North West > 292.5 - 337.5
################################################
appearsProcessed$windBearing <- apply(appearsProcessed, 1, transformWindBearingToFactorVariable)
appearsProcessed$windBearing <- as.factor(appearsProcessed$windBearing)

appearsProcessed$windBearing <- NULL

