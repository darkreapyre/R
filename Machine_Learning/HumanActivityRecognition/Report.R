## Initialize the environment
set.seed(6969)
library(caret) ## Hide output
if (!file.exists("data")) {
        dir.create("data")
}

## Download the Data
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(trainURL, "./data/train.csv", method = "curl")
download.file(testURL, "./data/test.csv", method = "curl")
downloadDate <- date()

## Read the file into R using read.table
trainData <- read.csv("./data/train.csv", header = TRUE)
testData <- read.csv("./data/test.csv", header = TRUE)

## Dimensions
dim(trainData)

# Select 8 random predictors
numPredictors <- ncol(trainData) - 1
s <- sample(1:numPredictors, 10)
Sample <- trainData[, s]
str(Sample)
summary(Sample)

## Plot the spread of missing values in the Sample
missingVar <- sapply(Sample, function(x) sum(is.na(x)))
dfmissing <- data.frame(variable = names(missingVar), missing = missingVar,
                        stringsAsFactors = FALSE)
dfmissing$variable <- factor(dfmissing$variable, levels = dfmissing$variable,
                             ordered =  FALSE)
qplot(x = variable, y = missing, data = dfmissing, geom = "bar", 
      stat = "identity", position = "dodge") +  
        xlab("Preictor Variable") + ylab("No. Missing Values") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
correlated <- findCorrelation(cor(trainData[, -120]), cutoff = 0.99)
trainData.reduced <- trainData[, -correlated]

## No. Predictors - 116
dim(trainData.reduced[, -117])[2]

##Subset the data for cross-validation (60/40 Split)
set.seed(6969)
subSet <- createDataPartition(y = trainData.reduced$classe, p = 0.6,
                              list = FALSE)
trainSet <- trainData.reduced[subSet, ]
valSet <- trainData.reduced[-subSet, ]

library(parallel) ## Hide
library(doParallel) ## Hide
library(randomForest) ## Warnings

## Create Parallel Cluster
cluster <- makeCluster(8)
registerDoParallel(cluster)

## Fit the Random Forest Model
rfModel <- randomForest(classe ~ ., data = trainSet, mtry = 13, ntree = 2048,
                        keep.forest = TRUE, importance = TRUE, proximity = TRUE)

## Plot the Important Variables
varImpPlot(rfModel)

## Plot the Model
plot(rfModel)
plot(margin(rfModel))
plot(outlier(rfModel), type = "h")

## Predict on the Validation data set
Predict <- predict(rfModel, valSet)

## Confusion Matrix
confusionMatrix(Predict, valSet$classe)

## Confusion Matrix - Table
confusionMatrix(Predict, valSet$classe)$table

## Confusion Martix - Overall Accuracy
confusionMatrix(Predict, valSet$classe)$overall[[1]]

## Out of Box Error rate
1 - confusionMatrix(Predict, valSet$classe)$overall[[1]]

## Apply Pre-processing methadology to the Test Data
removeCols <- c("X", "user_name", "raw_timestamp_part_1", "new_window", 
                "num_window", "raw_timestamp_part_2", "cvtd_timestamp")
testData <- testData[, !names(testData) %in% removeCols]
testData[is.na(testData)] <- 0
classe <- testData$problem_id
features <- sapply(testData, is.numeric)
testData <- cbind(testData[, features], classe)

## Final test (pml-testing.csv)
Final <- predict(rfModel, testData)

## Coursera Upload
pml_write_files = function(x) {
        n = length(x)
        for(i in 1:n) {
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i], file = filename, quote = FALSE,
                            row.names = FALSE, col.names = FALSE)
        }
}
pml_write_files(Final)
