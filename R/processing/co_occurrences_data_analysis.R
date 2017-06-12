
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

coocAppearsProcessed <- appearsProcessed[0,]
for (i in 1:nrow(appearedCategories)) {
  # Generate filter to obtain the rows matched with this combination
  combineFilter <- generateCombinationFilter(appearsProcessed, appearedCategories[i,])
  appearsMatched <- appearsProcessed[combineFilter,]
  # Sum all the cooc variables by columns
  appearsMatched[, coocMatches] <- lapply(appearsMatched[, coocMatches], sum)
  # Set to appears Processed variable
  coocAppearsProcessed <- rbind(coocAppearsProcessed, appearsMatched)
}

write.table(coocAppearsProcessed, file="cooc_appears_processed.csv")