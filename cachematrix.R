## Functions to create a matrix with cached inverse matrix for use in intense calculations
##

## This function accepts matrix as a parameter and allows to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(value) {
    x <<- value
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(value) inverse <<- value
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns cached inverse of the matrix or calculates and 
## caches inverse if none was calculated yet

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  inverse
}
