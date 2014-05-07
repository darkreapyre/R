corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  result <- numeric()
  for(file in list.files(directory)){
    data <- read.table(paste(directory, "/", file,sep=""), sep=",", header=TRUE)
    test <- nrow(data[complete.cases(data),])
    if(test > threshold){
      #total<-rbind(total,cor(pollutantData$nitrate,pollutantData$sulfate,use="complete.obs"))
      result <- c(result, cor(data$nitrate, data$sulfate, use="complete.obs"))
    }
  }
  ##round(as.numeric(result), digits = 5)
  as.numeric(result)
}
