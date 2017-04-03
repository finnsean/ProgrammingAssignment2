## A function which constructs on object consisting of a list of functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
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
  
  ## a function to get the inverse matrix
  ##getinv <- function() xInv
  getinv <- function(...) {
    if (!is.null(xInv)) {
      message("Returning a cached inverse matrix")
      return(xInv)
    }
    ## if exectution reaches here, no inverse exists and so one must be computed
    xInv <<- solve(x,...) ## compute the inverse, store in parent environment
    ## return the inverse
    xInv
  
  }
  
  ## compute a list of functions to be returned on 'construction'
  list(set = set, get = get,
       getinv = getinv)
  
}
