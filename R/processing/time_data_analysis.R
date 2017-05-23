####################### TIME DATA #################################################
## The information about the appeared time it's splitted in different variables
###################################################################################
### appearedLocalTime (continuous variable)
### appearedHour (continous variable, included in appeared Local time) 
### appearedMonth (character variable, included in appeared Local time)
### appearedYear (continous variable, included in appeared Local time)
### appearedDay (continous variable, included in appeared Local time)
### appearedDayOfWeek (factor variable, included in appeared Local time)
### appearedTimeOfDay (factor variable, can be extracted from appeared Local time)
###################################################################################

## Check different years (ignore if it's always the same)
unique(appears$appearedYear) # Only one value, so we can delete from dataset
appearsProcessed$appearedYear <- NULL

## Check different months (ignore if it's always the same)
unique(appears$appearedMonth) # Only one value, so we can delete from dataset
appearsProcessed$appearedMonth <- NULL

### Delete the continous variables of hour and minute which are described as time of day
### (May be we can factorize it?)
appearsProcessed$appearedHour <- NULL
appearsProcessed$appearedMinute <- NULL
appearsProcessed$appearedDay <- NULL

## Now let's take a look to the factor variables
summary(appearsProcessed$appearedDayOfWeek) #WTF dummy_day means? I suppose it's a NA
## We can get this info without NA's
appearsProcessed$appearedDayOfWeek <- apply(appearsProcessed, 1, defineDayOfWeek)
appearsProcessed$appearedDayOfWeek <- as.factor(appearsProcessed$appearedDayOfWeek)

summary(appearsProcessed$appearedTimeOfDay) #Looks good :)

### I think the appearedLocalTime is not a good variable to analyze because it's difficult to find occurences
appearsProcessed$appearedLocalTime <- NULL