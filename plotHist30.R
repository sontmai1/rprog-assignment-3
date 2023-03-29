
## 1 Plot the 30-day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay

## Plot the 30-day mortality rates for heart attack
hist(outcome[, 11])


