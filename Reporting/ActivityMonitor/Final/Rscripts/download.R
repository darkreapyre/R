download <- function(url) {
  rawdata <- data.frame(0)
  if(!file.exists("./data")) {dir.create("./data")}
  download.file(url, destfile = "./data/rawdata.csv")
#  result <- read.csv("./data/rawdata.csv",
#                     blank.lines.skip = T, skip = 4,
#                     nrows = 190)
  result <- read.csv("./data/rawdata.csv")
  return(result)
}

#colums <- c("shortname", "rank", "X2", "countryNames", "GDP", "X5", "X6", "X7", "X8", "X9")

