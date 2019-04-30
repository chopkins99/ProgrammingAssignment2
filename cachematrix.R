## makeCacheMatrix creates the matrix object which can cache its inverse
## cacheSolve returns the inverse of the matrix object


## This function creates a special "matrix" object that can cache its inverse
## "matrix" object is really a list containing a function to
##   1.  set the value of the matrix
##   2.  get the value of the matrix
##   3.  set the value of the inverse
##   4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseX) m <<- inverseX
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" 
##   returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
library(matlib)

  ## Check to see if inverse is already cached
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  ## Check to see if inverse exists (i.e. determinant != 0)
  if(det(data)==0) {
    message("inverse does not exist for matrix")
    return()
  }
  
  ## Use solve function to determine inverse
  m <- solve(data)
  x$setinverse(m)
  m
  
}
