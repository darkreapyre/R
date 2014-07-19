## Read the file into R using read.table
trainData <- read.csv("./data/train.csv", header = TRUE)
testData <- read.csv("./data/test.csv", header = TRUE)

## Dimensions
dim(trainData)

## Show NA's and (other) data
summary(trainData[, c(13, 17, 49, 105)])