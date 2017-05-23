### Urban -> urban + suburban + midurban
### Suburban -> suburban + midurban
### Rural <- rural
### MidUrban <- midurban
### This function returns the new category variable following the previous rules
defineUrban <- function(appear){
  if (appear["rural"] == "true") { 
    "rural"
  } else if (appear["midurban"] == "true" && appear["suburban"] == "true" && appear["urban"] == "true") {
    "urban"
  } else if (appear["midurban"] == "true" && appear["suburban"] == "true") {
    "suburban"
  } else {
    "midurban"
  }
}

defineDayOfWeek <- function(appear){
  date <- as.Date(appear["appearedLocalTime"], format='%Y-%m-%dT%H:%M:%S')
  format.Date(date,"%A")
}

#http://glcf.umd.edu/data/lc/
# TODO: improve this function
transformTerrainType <- function(appear) {
  terrainType <- as.integer(appear["terrainType"])
  if (terrainType == 0) "Water"
  else if (terrainType == 1) "Evergreen Needleleaf forest"
  else if (terrainType == 2) "Evergreen Broadleaf forest"
  else if (terrainType == 3) "Deciduous Needleleaf forest"
  else if (terrainType == 4) "Deciduous Broadleaf forest"
  else if (terrainType == 5) "Mixed forest"
  else if (terrainType == 6) "Closed shrublands"
  else if (terrainType == 7) "Open shrublands"
  else if (terrainType == 8) "Woody savannas"
  else if (terrainType == 9) "Savannas"
  else if (terrainType == 10) "Grasslands"
  else if (terrainType == 11) "Permanent wetlands"
  else if (terrainType == 12) "Croplands"
  else if (terrainType == 13) "Urban and built-up"
  else if (terrainType == 14) "Cropland/Natural vegetation mosaic"
  else if (terrainType == 15) "Snow and ice"
  else if (terrainType == 16) "Barren or sparsely vegetated"
  else "Unknown"
}

getFactorTime <- function(hours, minutes) {
  hoursChar <- as.character(hours)  
  if (minutes < 30) {
    hoursChar
  }
  else if (minutes <= 45) {
    paste(hoursChar, "30", sep=":")
  }
  else {
    nextHour <- as.integer(hours) + 1
    as.character(nextHour)
  }
}

getSunsetFactorTime <- function(appear) {
  getFactorTime(appear["sunsetHour"], appear["sunsetMinute"])
}

getSunriseFactorTime <- function(appear) {
  getFactorTime(appear["sunriseHour"], appear["sunriseMinute"])
}

tranformCloseToWaterToFactorVariable <- function(appear) {
  if (appear["closeToWater"] == "true") {
    "Yes"
  } else {
    "No"
  }
}

transformFactorToNumeric <- function(variable) {
  if (is.factor(variable)) {
    as.numeric(variable)
  }
  variable
}

tranformPokeStopDistanceToFactorVariable <- function(appear) {
  if (appear["pokestopIn100m"] == "true") { 
    "pokestopIn100m"
  } else if (appear["pokestopIn250m"] == "true") {
    "pokestopIn250m"
  } else if (appear["pokestopIn500m"] == "true") {
    "pokestopIn500m"
  } else if (appear["pokestopIn1000m"] == "true") {
    "pokestopIn1000m"
  } else if (appear["pokestopIn2500m"] == "true") {
    "pokestopIn2500m"
  } else if (appear["pokestopIn5000m"] == "true") {
    "pokestopIn5000m"
  } else {
    "pokestopIn+5000m"
  }
}

tranformGymDistanceToFactorVariable <- function(appear) {
  if (appear["gymIn100m"] == "true") { 
    "gymIn100m"
  } else if (appear["gymIn250m"] == "true") {
    "gymIn250m"
  } else if (appear["gymIn500m"] == "true") {
    "gymIn500m"
  } else if (appear["gymIn1000m"] == "true") {
    "gymIn1000m"
  } else if (appear["gymIn2500m"] == "true") {
    "gymIn2500m"
  } else if (appear["gymIn5000m"] == "true") {
    "gymIn5000m"
  } else {
    "gymIn+5000m"
  }
}

transformAtmosphericPressureToFactorVariable <- function(appear) {
  if (appear["pressure"] < 1005) "Low"
  else if (appear["pressure"] <= 1018) "Normal"
  else "High"
}

transformTemperatureToFactorVariable <- function(appear) {
  if (appear["temperature"] < 0) "Very Cold"
  else if (appear["temperature"] <= 10) "Cold"
  else if (appear["temperature"] <= 20) "Tempered"
  else if (appear["temperature"] <= 30) "Warm"
  else "Very Warm"
}


tranformWindSpeedToFactorVariable <- function(appear) {
  if (appear["windSpeed"] < 5) { 
    "Calm"
  } else if (appear["windSpeed"] <= 6) {
    "Light Air"
  } else if (appear["windSpeed"] <= 12) {
    "Light breeze"
  } else if (appear["windSpeed"] <= 19) {
    "Gentle breeze"
  } else if (appear["windSpeed"] <= 28) {
    "Moderate breeze"
  } else if (appear["windSpeed"] <= 38) {
    "Fresh breeze"
  } else if (appear["windSpeed"] <= 50) {
    "Strong breeze"
  } else {
    "High wind"
  }
}

transformWindBearingToFactorVariable <- function(appear) {
  if (appear["windBearing"] > 337.5 || appear["windBearing"] < 22.5) "North"
  else if (appear["windBearing"] < 67.5) "North-East"
  else if (appear["windBearing"] < 112.5) "East"
  else if (appear["windBearing"] < 157.5) "South-East"
  else if (appear["windBearing"] < 202.5) "South"
  else if (appear["windBearing"] < 247.5) "South-West"
  else if (appear["windBearing"] < 292.5) "West"
  else if (appear["windBearing"] < 337.5) "North-West"
}