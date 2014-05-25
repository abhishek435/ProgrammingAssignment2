## This function creates a list of matrix which
## has functions for storing and retrieval of the
## matrix and its inverse. It maintains an internal
## cache to store the original data (matrix) and its
## computed inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## Setter function for input matrix data
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Accessor funtion for input matrix data
  get <- function() x
  
  ## Setter function for inverse matrix of 'x'
  setinverse <- function(inverse) i <<- inverse
  
  ## Accessor funtion for inverse matrix of 'x'
  getinverse <- function() i
  
  ## Returns the created cacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a cached matrix created
## by function makeCacheMatrix and returns its
## inverse matrix by first checking the inverse
## is already calculated and store in the cache
## of the input matrix. If inverse value is already
## present, then the cached value is returned else
## inverse is calculated using the solve function
## store in the cache and returned as a result of 
## this function.
## It is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## First check in cache for inverse stored value
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Fetch the matrix whose inverse has to be calculated from input x
  data <- x$get()
  
  ## Calculate inverse of 'x'
  i <- solve(data, ...)
  
  ## Store the result into the cache
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
