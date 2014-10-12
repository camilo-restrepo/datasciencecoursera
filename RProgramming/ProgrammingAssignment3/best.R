best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states <- unique(data [,7])
  if(state %in% states == FALSE){
    stop("invalid state")
  }
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcomes == FALSE) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  filteredData <- subset(data, data[,7] == state)
  value <- 0
  if(outcome == "heart attack" ){
    filteredData <- subset(filteredData, filteredData[,11]!="Not Available")
    value <- which.min(filteredData[,11])
  }else if(outcome == "heart hailure"){
    filteredData <- subset(filteredData, filteredData[,17]!="Not Available")
    value <- which.min(filteredData[,17])
  }else{
    filteredData <- subset(filteredData, filteredData[,23]!="Not Available")
    value <- which.min(filteredData[,23])
  }
  filteredData[value,2]
}