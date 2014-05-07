rankhospital <- function(state, outcome, num = "best") {
        options(warn = -1)
        
        source_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        source_data[, 11] <- as.numeric(source_data[, 11])
        source_data[, 17] <- as.numeric(source_data[, 17])
        source_data[, 23] <- as.numeric(source_data[, 23])
        
        df <- data.frame(source_data[, 2], source_data[, 7], source_data[, 11], source_data[, 17], source_data[, 23])
        names(df) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
        
        s <- split(df, df$State)
        state_data <- s[[state]]
        
        if(!(state %in% names(s))) {
                stop("invalid state")
        }
        
        outcomes <- c("heart failure", "heart attack", "pneumonia")
        if(!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        
        result <- state_data[order(state_data[, outcome], state_data$Hospital), ]
        result <- na.omit(result)
        
        if(num == "best") {
                num <- 1
        } else if(num == "worst") {
                num <- nrow(result)
        }
        
        ranking <- as.numeric(num)
        result <- as.character(result[ranking, ]$Hospital)
        return(result)
}