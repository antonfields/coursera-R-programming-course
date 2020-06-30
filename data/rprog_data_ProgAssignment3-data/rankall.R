library(plyr)
library(dplyr)

rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        if (!state %in% unique(outcome_data$State))
                stop("invalid state")
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop("invalid outcome")
        
        ## For each state, find the hospital of the given rank
        
        # Change column names
        names(outcome_data)[11] <- "heart attack"
        names(outcome_data)[17] <- "heart failure"
        names(outcome_data)[23] <- "pneumonia"
        
        # Ensure outcome data is numeric
        outcome_data[11] <- suppressWarnings(apply(outcome_data[11], 2, as.numeric))
        outcome_data[17] <- suppressWarnings(apply(outcome_data[17], 2, as.numeric))
        outcome_data[23] <- suppressWarnings(apply(outcome_data[23], 2, as.numeric))
        
        # Sort by state, outcome, hospital name and add rank column
        ordered <-
                outcome_data %>%
                mutate(outcome_value = outcome_data[, outcome]) %>%
                arrange(State, outcome_data[, outcome], Hospital.Name) %>%
                group_by(State) %>%
                dplyr:: mutate(rank = row_number())
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        if(num == "best") num <- 1
        if(num == "worst") {
                states <- data.frame(state = unique(ordered$State))
                results <- 
                        ordered %>%
                        filter(!is.na(outcome_value)) %>%
                        filter(rank == n()) %>%
                        select(Hospital.Name, State)
                left_join(states, results, by = c("state" = "State")) %>%
                        select(Hospital.Name, state)
                return(results)
        }
        states <- data.frame(state = unique(ordered$State))
        results <- 
                ordered %>%
                filter(rank == num | rank == "NA") %>%
                select(Hospital.Name, State)
        left_join(states, results, by = c("state" = "State")) %>%
                select(Hospital.Name, state)
}
