## 2 Finding the best hospital in a state

best <- function(state, outcome) {

    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    if (any(outcome_data$State %in% state)) {
        
        valid_state_outcome <- outcome_data[outcome_data$State == state, ]
        
        lowest_rate_rows <- NULL

        if(outcome == "heart attack"){
            valid_state_outcome[, 11] <- as.numeric(valid_state_outcome[, 11])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            lowest_rate <- min(valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
            lowest_rate_rows <- valid_state_outcome[valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == lowest_rate, ]
        }
        
        else if(outcome == "heart failure"){
            #outcome_data[, 17] <- as.numeric(outcome_data[, 17])
            valid_state_outcome[, 17] <- as.numeric(valid_state_outcome[, 17])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            lowest_rate <- min(valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
            lowest_rate_rows <- valid_state_outcome[valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == lowest_rate, ]
        }
        else if(outcome == "pneumonia"){
            # outcome_data[, 23] <- as.numeric(outcome_data[, 23])
            valid_state_outcome[, 23] <- as.numeric(valid_state_outcome[, 23])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            lowest_rate <- min(valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
            lowest_rate_rows <- valid_state_outcome[valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == lowest_rate, ]
        }

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if (is.null(lowest_rate_rows)) {
            message(paste0( "Error in best(", state , ",", outcome ,"): invalid outcome"))
        } 
        else {
            # sort by Hospital Name
            sorted_result <- lowest_rate_rows[order(lowest_rate_rows$Hospital.Name),]
            
            sorted_result[1,]$Hospital.Name
        }
    }
    else{
        message(paste0( "Error in best(", state , ",", outcome ,"): invalid state"))
    }
}
