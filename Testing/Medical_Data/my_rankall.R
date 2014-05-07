rankall <- function(outcome, num = "best") {
        options(warn = -1)
        
        source_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        source_data[, 11] <- as.numeric(source_data[, 11])
        source_data[, 17] <- as.numeric(source_data[, 17])
        source_data[, 23] <- as.numeric(source_data[, 23])
        
        df <- data.frame(source_data[, 2], source_data[, 7], source_data[, 11], source_data[, 17], source_data[, 23])
        names(df) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
        
        s <- split(df, df$State)
        ##state_data <- 
        
        outcomes <- c("heart failure", "heart attack", "pneumonia")
        if(!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        x <- character(0)
        y <- names(s)
        for(i in names(s)) {
                
                state_data <- s[[i]]
                result <- state_data[complete.cases(state_data[, outcome]), ]
                result <- result[order(result[, outcome], result$Hospital), ]
                ##result <- na.omit(result)
                ##result <- state_data[order(state_data[, outcome], state_data$Hospital), ]
                if(num == "best") {
                        ranking <- 1
                } else if(num == "worst") {
                        ranking <- nrow(result)
                } else {
                        ranking <- as.numeric(num)
                }
                x <- c(x, as.character(result[ranking, ]$Hospital))
        }
        return(data.frame(hospital = x, state = y))
        ## return(x)
        
}