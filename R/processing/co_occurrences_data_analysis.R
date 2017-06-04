
## Get coo ocurrence columns
coocMatches <- subset(appearColNames, grepl("cooc_", appearColNames))

# Transform co occurences to a numeric values
appearsProcessed[, coocMatches] <- lapply(appearsProcessed[, coocMatches], function(occurrence) {
  as.integer(as.logical(occurrence))
})
  
# Delete coo ocurrence columns and put into another variable
appearedCategories <- appearsProcessed
appearedCategories[, coocMatches] <- NULL

# Delete repeated values
appearedCategories <- unique(appearedCategories)

by(appearedCategories, 1:nrow(appearedCategories), function(row, appears){
  # Generate filter to obtain the rows matched with this combination
  combineFilter <- generateCombinationFilter(appears, row)
  appearsMatched <- appears[combineFilter,]
  # Sum all the cooc variables by columns
  appearsMatched[, coocMatches] <- lapply(appearsMatched[, coocMatches], sum)
  # Set to appears Processed variable
  appears[combineFilter,] <- appearsMatched

}, appears = appearsProcessed)