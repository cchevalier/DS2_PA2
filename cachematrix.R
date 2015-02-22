## This R script contains a pair of functions that cache the inverse of a matrix.
## 
## Programming Assignment 2 for R Programming course on coursera.org
## February 2015
##
##
## Usage:
##
##  > m <- makeCacheMatrix(rbind(c(2, 0), c(0, 2)))
##  > m$getinv()
##  NULL
##
##  > cacheSolve(m)
##       [,1] [,2]
##  [1,]  0.5  0.0
##  [2,]  0.0  0.5
##
##  > cacheSolve(m)
##  returning the cached inverse matrix
##       [,1] [,2]
##  [1,]  0.5  0.0
##  [2,]  0.0  0.5
##
##  > m$getinv()
##       [,1] [,2]
##  [1,]  0.5  0.0
##  [2,]  0.0  0.5
##


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # we originally cache the inverse matrix minv as NULL
  minv <- NULL
  
  # Set function for the matrix
  set <- function(y) {
    x <<- y
    # reset minv to null
    minv <<- NULL
  }
  
  # get function for the matrix
  get <- function() x
  
  # set function for the inverse matrix
  setinv <- function(inv) minv <<- inv
  
  # get function for the inverse matrix
  getinv <- function() minv
  
  # Package all functions into a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # First retrieve the cached inverse matrix
  minv <- x$getinv()
  
  # If minv is not null then return the cached value as the inverse matrix
  if(!is.null(minv)) {  
    message("returning the cached inverse matrix")
    return(minv)
  }
  
  # Compute the inverse matrix using solve() then cache the result and finally return it
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}
