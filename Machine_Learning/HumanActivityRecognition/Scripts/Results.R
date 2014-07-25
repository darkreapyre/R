## Apply Pre-processing methadology to the Test Data
## Remove unnecessary Columns
removeCols <- c("X", "user_name", "raw_timestamp_part_1", "new_window", 
                "num_window", "raw_timestamp_part_2", "cvtd_timestamp")
testData <- testData[, !names(testData) %in% removeCols]

## Chnage all the missing values to ZERO
testData[is.na(testData)] <- 0

## Ensure all the data that can be used as features (numeric class)
classe <- testData$problem_id
features <- sapply(testData, is.numeric)
testData <- cbind(testData[, features], classe)

## Final test (pml-test.csv)
Final <- predict(rfModel, testData)
## If using train()
##Final <- predict(modelFit, testData)

## Coursera Upload
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(Final)
