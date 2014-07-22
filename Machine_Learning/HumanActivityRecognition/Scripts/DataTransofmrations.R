#############################################################################
##                 Data Transofmations and Pre-Processing                 ##
############################################################################

## Remove unnecessary Columns
removeCols <- c("X", "user_name", "raw_timestamp_part_1", "new_window", 
                "num_window", "raw_timestamp_part_2", "cvtd_timestamp")
trainData <- trainData[, !names(trainData) %in% removeCols]

## Create an index of the columns that can be used as features (numeric class)
features <- lapply(trainData, class) %in% "numeric"
featureIndex <- which(features)

## Replace the missing data with the values of nearest neighbor 
set.seed(6969)
imputed <- preProcess(trainData[, featureIndex], method = "knnImpute")
trainData.imputed <- predict(imputed, trainData[, featureIndex])
trainData.imputed$classe <- trainData$classe

## Find correlations in the data for further dimentionality reduction
correlated <- findCorrelation(cor(trainData.imputed[, -89]), cutoff = 0.9)
trainData.reduced <- trainData.imputed[, -correlated]

## 57 Pedictors
dim(trainData.reduced[, -58])[2]

##Subset the data for cross-validation (60/40 Split)
set.seed(6969)
subSet <- createDataPartition(y = trainData.reduced$classe, p = 0.6,
                              list = FALSE)
trainSet <- trainData.reduced[subSet, ]
testSet <- trainData.reduced[-subSet, ]