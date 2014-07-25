library(parallel)
library(doParallel)
library(randomForest)
library(e1071)

## Create Parallel Cluster
cluster <- makeCluster(8)
registerDoParallel(cluster)

## Model 1 - KNN = (0.89)
set.seed(6969)
modelKNN <- train(trainSet$classe ~ ., data = trainSet, method = "knn")

## Model 2 - Random Forest Model = (0.98)
set.seed(6969)
modelRF <- train(trainSet$classe ~ ., data = trainSet, method = "rf",
                 prox = TRUE)

## Model 3 - Recursive Partitioning = (0.46)
set.seed(6969)
modelRpart <- train(trainSet$classe ~ ., data = trainSet, method = "rpart")

## Model 4 - Linear Discriminate Analysis = (0.591)
set.seed(6969)
modelLda <- train(trainSet$classe ~ ., data = trainSet, method = "lda")



## Apply the above with Cross-validation
## Model 1 - KNN = (0.91)
set.seed(6969)
Ctrl <- trainControl(method = "cv")
modelKNN2 <- train(trainSet$classe ~ ., data = trainSet, method = "knn",
                   trControl = Ctrl)

## Model 2 - Random Forest = (0.985)
set.seed(6969)
Ctrl <- trainControl(method = "cv")
modelRF2 <- train(trainSet$classe ~ ., data = trainSet, method = "rf",
                  trControl = Ctrl, prox = TRUE)

## Model 3 - Recursive Partitioning = (0.49)
set.seed(6969)
Ctrl <- trainControl(method = "cv")
modelRpart2 <- train(trainSet$classe ~ ., data = trainSet, method = "rpart",
                     trControl = Ctrl)

## Model 4 - Linear Discriminate Analysis = (0.594)
set.seed(6969)
Ctrl <- trainControl(method = "cv")
modelLda2 <- train(trainSet$classe ~ ., data = trainSet, method = "lda",
                   trControl = Ctrl)

## The Model to aply is Model 2 (Random Forest) with Cross-Validation!
