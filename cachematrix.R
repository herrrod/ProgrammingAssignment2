## A pair of functions for caching matrix inversion calculation to improve 
## performance of subequent retrieval.

## Create a special matrix that contains functions for caching and retrieval 
## of its inverse.
## Args:
##    x: a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of the supplied matrix created from "makeCacheMatrix". 
## The first time this function is called the inverse is calculated and cached. 
## Subsequent calls to this function with the same matrix will return the cached
## inverse.
## Args:
##    x: a matrix created by function makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
