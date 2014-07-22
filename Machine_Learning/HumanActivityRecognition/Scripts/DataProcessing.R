#############################################################################
##                        Data Processing                                 ##
############################################################################

## Initialize the environment
library(caret) ## Hide output
library(RANN)
if (!file.exists("data")) {
        dir.create("data")
}

## Download the Data
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
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
set.seed(6969)
s <- sample(1:numPredictors, 10)
Sample <- trainData[, s]
str(Sample)
summary(Sample)


## Plot the splead of missing values in the Sample
missingVar <- sapply(Sample, function(x) sum(is.na(x)))
dfmissing <- data.frame(variable = names(missingVar), missing = missingVar,
                        stringsAsFactors = FALSE)
dfmissing$variable <- factor(dfmissing$variable, levels = dfmissing$variable,
                              ordered =  FALSE)
p <- qplot(x = variable, y = missing, data = dfmissing, geom = "bar", 
           stat = "identity", position = "dodge") +  
        xlab("Preictor Variable") + ylab("No. Missing Values") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))