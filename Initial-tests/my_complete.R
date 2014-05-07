complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## Create empty table
  df <- data.frame()
  
  ## Loop through the files (by id to 3 places) and pobulate the data frame.
  
  for(file in rep(id)){
    ## read the data from the file locations.
    data <- read.table(paste(directory,"/",sprintf("%03d", file),".csv",sep=""),sep=",",header=TRUE)
    
    df <- rbind(df,cbind(file,nrow(data[complete.cases(data),])))
  
  }
  
  ## Name the Colums from the result
  colnames(df)<-c("id","nobs")
  
  ## Retun the resultant Data Frame
  df
}
