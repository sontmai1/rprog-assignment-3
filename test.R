############## TEST: best.R ######################

source("best.R")

best("TX", "heart attack")

best("TX", "heart failure")

best("MD", "heart attack")

best("MD", "pneumonia")

best("BB", "heart attack")

best("NY", "hert attack")


############## TEST: rankhospital.R ######################

source("rankhospital.R")

rankhospital("TX", "heart attack")

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MD", "heart attack")
rankhospital("MN", "heart attack", 5000)
rankhospital("GA", "heart attack")

