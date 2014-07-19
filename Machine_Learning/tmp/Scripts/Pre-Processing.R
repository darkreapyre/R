## Convert the features that can be, to numeric data
features <- lapply(trainData, class) %in% "numeric"

## Create an index of the columns that can be used as features
featureIndex <- which(features)

## Replace the missing data with the values of nearest neighbor
set.seed(6969)
imputed <- preProcess(trainData[, featureIndex], method = "knnImpute")
trainData.imputed <- predict(imputed, trainData[, featureIndex])
trainData.imputed$classe <- trainData$classe

##Subset the data for model testing/validation (60/40 Split)
set.seed(6969)
subSet <- createDataPartition(y = trainData.imputed$classe, p = 0.6, list = FALSE)
trainSet <- trainData.imputed[subSet, ]
valSet <- trainData.imputed[-subSet, ]