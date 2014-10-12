cube <- function(x, n){
  x^3
}

otra <- function(){
  x<-1:10
  if(x>5){
    x<-0
  }
}


f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}