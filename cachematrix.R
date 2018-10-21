# Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## This function creates a special matrix object that is a list containing a function to
## set the value of matrix, get the value of matrix, get the inverse of matrix and set the
## inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y){
    x <<- x
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) matinv <<- inv
  getinv <- function() invmat
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the inverse of a matrix object created by makeCacheMatrix() function. 
## If the inverse has already been calculated (and the matrix has not changed), then it fetches
## the inverse from the cache, otherwise it calculates the inverse.
cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  return(invmat)
}
