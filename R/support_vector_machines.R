# https://www.svm-tutorial.com/2014/10/support-vector-regression-r/
library(e1071)
appearsDF <- as.data.frame(appearsProcessed)

## model 1: LINEAR kernel, C=1 (cost parameter)
(model <- svm(appearsDF[,-1],appearsDF[,1], type="C-classification", cost=1, kernel="linear", scale = FALSE))