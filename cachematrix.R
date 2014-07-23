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

### testing the above functions

message("starting tests")
message("creating 1000 x 1000 matrix")
testmatrix <- replicate(1000, rnorm(1000))

message("solving matrix")
testinverse <- solve(testmatrix)

message("caching matrix")
cachedmatrix <- makeCacheMatrix(testmatrix)

message("solving cached matrix 1st time")
start <- Sys.time()
inverse <- cacheSolve(cachedmatrix)
end <- Sys.time()
message (paste("execution took", end - start, "seconds"))

message("solving cached matrix 2nd time")
start <- Sys.time()
inverse <- cacheSolve(cachedmatrix)
end <- Sys.time()
message (paste("execution took", end - start, "seconds"))

message("comparing cached solution with non-cached")
if (isTRUE(all.equal(testinverse, inverse))) {
  message("matrices are equal, passed successfully")
} else {
  error("matrix equality test failed")
}

message("tests completed")

rm(list=ls())
