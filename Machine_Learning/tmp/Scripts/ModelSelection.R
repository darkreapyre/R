## Find correlations in the data by creating a correlation matrix
CorMatrix <- abs(cor(sapply(trainSet, unclass)))
diag(CorMatrix) <- 0

## Find the number of colums correlations above 0.75
sum(abs(CorMatrix[upper.tri(CorMatrix)]) > 0.75)/length(CorMatrix) #1%

## Some Model selction using glmulti
## require(glmulti)
##findModel <- glmulti(y ~ ., data = trainSet, maxit = 30)

