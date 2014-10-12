complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  idLength <- length(id)
  resultData <- rep(0, idLength)
  files <- as.character(list.files(directory))
  paths <- paste(directory, files, sep = "/")
  j<-1
  for(i in id){
    current <- read.csv(paths[i], header = TRUE, sep=",")
    resultData[j] <- sum(complete.cases(current))
    j<-j+1
  }
  resultData <-data.frame(id=id, nobs = resultData)
  return(resultData)
}