
rankhospital <- function(state, outcome, num = "best") {
    
    # state = "TX"
    # outcome = "heart failure"
    # num = 4
    
    
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    if (any(outcome_data$State %in% state)) {
        
        valid_state_outcome <- outcome_data[outcome_data$State == state, ]
        
        if(outcome == "heart attack"){
            valid_state_outcome[, 11] <- as.numeric(valid_state_outcome[, 11])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        }
        
        else if(outcome == "heart failure"){
            #outcome_data[, 17] <- as.numeric(outcome_data[, 17])
            valid_state_outcome[, 17] <- as.numeric(valid_state_outcome[, 17])
            valid_state_outcome <- na.omit(valid_state_outcome)
            
            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        }
        else if(outcome == "pneumonia"){
            # outcome_data[, 23] <- as.numeric(outcome_data[, 23])
            valid_state_outcome[, 23] <- as.numeric(valid_state_outcome[, 23])
            valid_state_outcome <- na.omit(valid_state_outcome)

            rank_rate_state <- data.frame(Hospital.Name = valid_state_outcome$Hospital.Name, Rate = valid_state_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)            
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if (is.null(rank_rate_state)) {
            message(paste0( "Error in rankhospital(", state , ",", outcome ,",", num ,"): invalid outcome"))
        } 
        else {
            # sort by Hospital Name
            sorted_result <- rank_rate_state[order(rank_rate_state$Rate),]
            

            if (is.numeric(num)) {
                
                # print("my_num is a numeric value.")
                rank <- as.numeric(num)
                # print(sorted_result[rank,]$Hospital.Name)
                result <- sorted_result[rank,]$Hospital.Name
                result
                
                
            } else {
                # print("my_num is not a numeric value: ")
                
                if(num == "best") {
                    # print("rank == best ...")
                    
                    # sort by Hospital Name increasing
                    
                    result <- sorted_result[1,]$Hospital.Name
                    # print(result)
                    result
                }
                else if(num == "worst") {
                    # print("rank == worst ...")
                    
                    # sort by Hospital Name decreasing
                    # sorted_result <- lowest_rate_rows[order(decreasing = TRUE, lowest_rate_rows$Hospital.Name),]
                    # sorted_result[1,]$Hospital.Name
                    result <- tail(sorted_result, n = 1)$Hospital.Name
                    # print(result)
                    result
                }
                else {
                    message(paste0( "Error in rankhospital(", state , ",", outcome ,",", num ,"): invalid num"))
                }
            }
        }
    }
    else{
        message(paste0( "Error in rankhospital(", state , ",", outcome ,",", num ,"): invalid state"))
    }
    
}
