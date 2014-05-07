best <- function(state, outcome) {
        ## Read the outcome data
        source_data <- read.csv("outcome-of-care-measures.csv", 
                                colClasses = "character")
        options(warn = -1)
        
        ## Change colum class
        source_data[, 11] <- as.numeric(source_data[, 11])
        source_data[, 17] <- as.numeric(source_data[, 17])
        source_data[, 23] <- as.numeric(source_data[, 23])
              
        ## Extract specific data to work with
        df <- data.frame(source_data[, 2], source_data[, 7], source_data[, 11],
                         source_data[, 17], source_data[, 23])
        names(df) <- c("hospital", "state", "heart attack", "heart failure",
                       "pneumonia")
        
        ## ----------------------------------Origional attemp---------------------------------------------
        ## Catagorize by State
        ##s <- split(df, df$state)
        ##list_data <- s[state] ## list_data is a list (key = state)
        ##state_data <- list_data[[1]] ## state_data is the value
        
        ## Catatgorize by state
        s <- split(df, df$state) ## KEY = State
        state_data <- s[[state]] ## data frame of state specific data 
        
        ##Check that state and outcome are valid
        ##if(state_data == NULL) {
        ##        stop("invalid state")
        ##}
        
        if(!(state %in% s[1])) {
                stop("invalid state")
        }
        
        outcomes <- c("heart failure", "heart attack", "pneumonia")
        if(!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        
        ## Return the hospital name in that state with the lowest 30-day death 
        ## rate
        rowIndex <- which.min(state_data[, outcome]) ## get the row number where the minimum occurs
        
        ## NOTE: Keep NA valus for indexing
        
        result <- state_data[rowIndex, ]$hospital ## Get the hosspital at the row index where the minimum occure
        result <- as.character(result) ## output is factor, convert to character
        return(result)        
}