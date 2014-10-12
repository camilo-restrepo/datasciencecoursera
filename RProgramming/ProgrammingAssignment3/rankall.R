rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcomes == FALSE) {
    stop("invalid outcome")
  }
  if( num != "best" && num != "worst" && num%%1 != 0 ){
    stop("invalid num")
  } 
  ## For each state, find the hospital of the given rank
  states <- unique(data [,7])

  if(outcome == "heart attack" ){
    column <- 11
  }else if(outcome == "heart hailure"){
    column <- 17
  }else{
    column <- 23
  }
  
  filteredData <- subset(data, data[,column]!="Not Available")
  filteredData[,column] <- as.numeric(filteredData[,column])
  filteredData  <- filteredData[order(filteredData[,2], decreasing = FALSE), ]
  filteredData  <- filteredData[order(filteredData[,column], decreasing = FALSE), ]
  
  newdata <- data.frame("hospital"=character(), "state"=character())
  for(state in states){
    stateData <- subset(filteredData, filteredData[,7] == state)
    if(num == "best"){
      value <- which.min(stateData[,column])
    }else if(num == "worst"){
      value <- which.max(stateData[,column])
    }else{
      value <- num
    }
    newdata <- rbind(newdata, data.frame(stateData[value,2], stateData[value,7]))
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  colnames(newdata) <- c("hospital", "state")
  newdata
}
