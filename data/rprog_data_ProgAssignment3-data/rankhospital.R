library(plyr)

rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        if (!state %in% unique(outcome_data$State))
                stop("invalid state")
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop("invalid outcome")
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        # Change column names
        names(outcome_data)[11] <- "heart attack"
        names(outcome_data)[17] <- "heart failure"
        names(outcome_data)[23] <- "pneumonia"
        
        # Ensure outcome data is numeric
        outcome_data[11] <- apply(outcome_data[11], 2, as.numeric)
        outcome_data[17] <- apply(outcome_data[17], 2, as.numeric)
        outcome_data[23] <- apply(outcome_data[23], 2, as.numeric)
        
        # Subset by state and then sort the data frame
        state_data <- outcome_data[outcome_data$State == state,]
        state_data <- state_data[complete.cases(state_data[,outcome]),]
        ordered <- arrange(state_data, state_data[,outcome], Hospital.Name)
        
        # Select the state to return
        last_row <- nrow(ordered)
        
        if(num == "best") {
                ref = 1 
        } else if(num == "worst") {
                ref = last_row
        } else {
                ref = num
        }
        
        ordered[ref,2]
        
}




