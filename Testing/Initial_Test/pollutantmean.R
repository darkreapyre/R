pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## Create the Data Frame container.
  df <-data.frame()
  
  ## Loop through the files (by id to 3 places) and pobulate the data frame.
  for(file in rep(id)){
    df <-rbind(df, read.table(paste(directory, "/", sprintf("%03d", file), ".csv", sep=""), sep=",", header=TRUE))
  }
  
  ## Calculate the Mean of the pullutant column as a Matrix and remove the missing values.
  mean(as.matrix(df[pollutant]), na.rm = TRUE)
  
  ## Resultant Mean is 7 digits long, so format the result to 3 decimal spaces.
  ##round(mean(as.matrix(df[pollutant]),na.rm=TRUE), digits = 3)
  
}