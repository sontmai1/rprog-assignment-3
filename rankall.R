## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

rankall <- function(outcome, num = "best") {

    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    valid_States <- unique(outcome_data$State)
    
    # create an empty data frame to store the results
    hospitals_states <- data.frame(Hospital = character(), State = character(), stringsAsFactors = FALSE)
    
    for (state in valid_States) {

        valid_state_outcome <- outcome_data[outcome_data$State == state, ]
        
        if(outcome == "heart attack"){
            valid_state_outcome[, 11] <- as.numeric(valid_state_outcome[, 11])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, 
                                          Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        }
        
        else if(outcome == "heart failure"){
            #outcome_data[, 17] <- as.numeric(outcome_data[, 17])
            valid_state_outcome[, 17] <- as.numeric(valid_state_outcome[, 17])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, 
                                          Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        }
        else if(outcome == "pneumonia"){
            # outcome_data[, 23] <- as.numeric(outcome_data[, 23])
            valid_state_outcome[, 23] <- as.numeric(valid_state_outcome[, 23])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, 
                                          Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)            
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if (is.null(rank_rate_state)) {
            stop("invalid outcome")
        } 
        else {
            # sort by Rate, and Hospital.Name
            sorted_result <- rank_rate_state[order(rank_rate_state$Rate, rank_rate_state$Hospital.Name),]
            
            if (is.numeric(num)) {
                rank <- as.numeric(num)
                state_hospital <- sorted_result[rank,]$Hospital.Name
                state_hospital
                
            } else {
                
                if(num == "best") {
                    state_hospital <- sorted_result[1,]$Hospital.Name
                    state_hospital
                }
                else if(num == "worst") {
                    state_hospital <- tail(sorted_result, n = 1)$Hospital.Name
                    state_hospital
                }
                else {
                    stop("invalid num")
                }
            }
        }
        
        hospitals_states <- rbind(hospitals_states, data.frame(Hospital = state_hospital, 
                                                               State = state, 
                                                               stringsAsFactors = FALSE))
    }
    # sort by State and return final result
    final_result <- hospitals_states[order(hospitals_states$State),]
}

