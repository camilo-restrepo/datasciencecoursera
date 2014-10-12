## Functions that cache a matrix inverse

## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Inverse property
  inv <- NULL
  
  ## This function sets the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## This function returns the matrix
  get <- function() x
  
  ## This function sets the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## This function returns the inverse of the matrix
  getInverse <- function() inv
  
  ## List of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of a matrix
## It first checks if the inverse has already been calculated
## It gets the inverse from a cache and skips computation
## If the inverse hasnt been calculated it computes it and store it in the cache
cacheSolve <- function(x, ...) {
  
  ## Returns the inverse of x
  inv <- x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  
  ##If the inverse is not cached the it computes it and caches it
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
