## 3. Ranking hospitals by outcome in a state
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    if (any(outcome_data$State %in% state)) {
        
        valid_state_outcome <- outcome_data[outcome_data$State == state, ]
        
        if(outcome == "heart attack"){
            valid_state_outcome[, 11] <- as.numeric(valid_state_outcome[, 11])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, 
                                          Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        }
        
        else if(outcome == "heart failure"){
            
            valid_state_outcome[, 17] <- as.numeric(valid_state_outcome[, 17])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, 
                                          Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        }
        else if(outcome == "pneumonia"){
            
            valid_state_outcome[, 23] <- as.numeric(valid_state_outcome[, 23])
            valid_state_outcome <- na.omit(valid_state_outcome)

            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, 
                                          Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)            
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        
        if (is.null(rank_rate_state)) {
            stop("invalid outcome")
        } 
        else {
            
            # sort by Rate, and Hospital.Name
            sorted_result <- rank_rate_state[order(rank_rate_state$Rate, rank_rate_state$Hospital.Name),]
            
            if (is.numeric(num)) {
                
                rank <- as.numeric(num)
                result <- sorted_result[rank,]$Hospital.Name
                result
                
            } else {
                
                if(num == "best") {
                   
                    result <- sorted_result[1,]$Hospital.Name
                    result
                }
                else if(num == "worst") {
                    result <- tail(sorted_result, n = 1)$Hospital.Name
                    result
                }
                else {
                    stop("invalid num")
                }
            }
        }
    }
    else{
        stop("invalid state")
    }
    
}
