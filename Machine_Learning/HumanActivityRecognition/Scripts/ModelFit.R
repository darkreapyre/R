library(parallel) ## Hide
library(doParallel) ## Hide
library(randomForest) ## Warnings

## Create Parallel Cluster
cluster <- makeCluster(8)
registerDoParallel(cluster)

#########################################################################################

## Fit the Random Forest Model to the data, using cross-validation
##set.seed(6969)
##trainCtrl <- trainControl(method = "cv")
##modelFit <- train(trainSet$classe ~ ., data = trainSet, method = "rf",
##                  trControl = trainCtrl, prox = TRUE, importance = TRUE)

#########################################################################################

## Fit the Random Forest Model (not using caret)
## Find the optimal number of variables to try split on
##set.seed(6969)
##findMtry <- tuneRF(trainSet[, -58], trainSet$classe, ntreeTry = 2048,
##                   stepFactor = 1.5, improve = 0.01, trace = TRUE,
##                   plot = TRUE, dobest = FALSE)

## Fit the Random Forest Model
rfModel <- randomForest(classe ~ ., data = trainSet, mtry = 13, ntree = 2048,
                        keep.forest = TRUE, importance = TRUE, proximity = TRUE)


## Plot the Important Variables
varImpPlot(rfModel)

## Predict on the Validation data set
Predict <- predict(rfModel, valSet)

## Confusion Matrix
confusionMatrix(Predict, valSet$classe)

## Confusion Matrix - Table
confusionMatrix(Predict, valSet$classe)$table

## Confusion Martix - Overall Accuracy (0.9934)
confusionMatrix(Predict, valSet$classe)$overall[[1]]
## OR this with postResample from {caret}
##x <- postResample(Predict, valSet$classe)[[1]]

## Out of Box Error rate
1 - confusionMatrix(Predict, valSet$classe)$overall[[1]]
