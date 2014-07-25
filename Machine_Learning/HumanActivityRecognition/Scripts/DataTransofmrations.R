#############################################################################
##                 Data Transofmations (Set to Zero)                      ##
############################################################################

## Remove unnecessary Columns
removeCols <- c("X", "user_name", "raw_timestamp_part_1", "new_window", 
                "num_window", "raw_timestamp_part_2", "cvtd_timestamp")
trainData <- trainData[, !names(trainData) %in% removeCols]

## Chnage all the missing values to ZERO
trainData[is.na(trainData)] <- 0

## Ensure all the data that can be used as features (numeric class)
classe <- trainData$classe
features <- sapply(trainData, is.numeric)
trainData <- cbind(trainData[, features], classe)

## Find correlations in the data for further dimentionality reduction
correlated <- findCorrelation(cor(trainData[, -120]), cutoff = 0.9)
trainData.reduced <- trainData[, -correlated]

## No. Predictors
dim(trainData.reduced[, -1])[2]

##Subset the data for cross-validation (60/40 Split)
set.seed(6969)
subSet <- createDataPartition(y = trainData.reduced$classe, p = 0.6,
                              list = FALSE)
trainSet <- trainData.reduced[subSet, ]
valSet <- trainData.reduced[-subSet, ]
