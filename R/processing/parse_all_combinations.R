#Read Appeared Combinations from RDS
################################################################

appearedCombinations <- readRDS("/data/models/appearedCombinations.rds")

appearedCombinations$terrainType <- apply(appearedCombinations, 1, function(row) {
  gsub("/", "-", row['terrainType'])
})

saveRDS(appearedCombinations, "../appearedCombinations.rds")
