## These functions create an R object that maintains a 
## matrix with a cacheable inverse and the equivalent
## solve function for calculating (and caching) the inverse
 
## makeCacheMatrix takes a square matrix x and returns
## a list that contains get/set functions for the 
## matrix itself and its inverse
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve takes an R object (list) created by
## makeCacheMatrix and returns its inverse.
## Internally, if the inverse has already been
## calculated it returns the cached value otherwise
## it calculates the inverse and caches for future
## calls
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  m <- x$get()
  minv <- solve(m, ...)
  x$setinv(minv)
  minv
}