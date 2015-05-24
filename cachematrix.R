## Script used to encapsulate a matrix with contains its own inverse. 
## The purpose is keep the inverse stored with its already being 
## calculated and the matrix is not changed, avoiding unecessary
## calculation.

## Function that creates a list which encapsulates a matrix 
## into a list structure, that presents the method elements:
## set - assign a informed matrix
## get - returns the matrix
## setInverse - set the inverse of the matrix
## getInverse - get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(pinv) inv <<- pinv
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}

## Perform the inverse calculation of a "encapsulated" 
## matrix x, which is generated through the function 
## makeCacheMatrix, and stores the calculated inverse into x.
## If the inverse was already calculated, the calculation is 
## not performed.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (is.null(inv))
  {
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)  
  }
  inv
}
