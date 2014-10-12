rankhospital <- function(state, outcome, num = "best") {
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
  
  if( num != "best" && num != "worst" && num%%1 != 0 ){
    stop("invalid num")
  } 
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  filteredData <- subset(data, data[,7] == state)

  if(outcome == "heart attack" ){
    column <- 11
  }else if(outcome == "heart hailure"){
    column <- 17
  }else{
    column <- 23
  }

  filteredData <- subset(filteredData, filteredData[,column]!="Not Available")
  filteredData[,column] <- as.numeric(filteredData[,column])
  filteredData  <- filteredData[order(filteredData[,2], decreasing = FALSE), ]
  filteredData  <- filteredData[order(filteredData[,column], decreasing = FALSE), ]
  
  if(num == "best"){
    value <- which.min(filteredData[,column])
  }else if(num == "worst"){
    value <- which.max(filteredData[,column])
  }else{
    value <- num
  }
  filteredData[value,2]
}