## A function which constructs on object consisting of a list of functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix inverse
## 4) get the value of the matrix inverse
## 
## A call to makeCacheMatrix() or set() associates an R matrix object with
## the makeCacheMatrix object
## 
## A call to getinv() returns the inverse of the matrix.
## The inverse of the matrix is computed only if required, 
## and cached. If a cached solution exists, this is returned

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL; ## x_inv stores the matrix inverse. Initialised to NULL
  
  ## a function to set the matrix
  set <- function(y) {
    x <<- y;       ## set the matrix in the parent environment
    xInv <<- NULL; ## reset the inverse matrix to NULL
  }
  
  ## a function to return the matrix
  get <- function() x
  
  ## a function to set the inverse matrix
  setinv <- function(inv) xInv <<- inv
  
  ## a function to get the inverse matrix
  getinv <- function() xInv
  
  ## compute a list of functions to be returned on 'construction'
  list(set = set, get = get,
       getinv = getinv,
       setinv = setinv)
  
}

## This function calculates the inverse of the special "matrix" created 
## with the above function. 
## A call to this function computes the inverse matrix, but if 
## an inverse has already been computed and chached, this value
## is returned instead.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
