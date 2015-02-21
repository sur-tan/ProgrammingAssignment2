## The following functions computes and caches the inverse of a matrix.
## It consists of two functions: makeCacheMatrix and cacheSolve


## This function creates a special "matrix" object that cache its inverse.
## It constains the following functions:
## set: set the value of the matrix
## get: get the value of the matrix
## setinverse: set the value of the inverse matrix 
## getinverse: get the value of the inverse matrix
##
## Usage: makeCacheMatrix(x)
## Arguments: x is a matrix
## Return: a special "matrix" of x 

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  
  # set the value of the matrix and reset the inverse value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of inverse matrix 
  setinverse <- function(i) inv <<- i
  
  # get the value of the inverse matrix
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

 

## This function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function. It will retrieve the
## inverse from the cache if it has already been calculated.
## 
## Usage: cacheSolve(x)
## Arguments: x is the special "matrix" to compute the inverse. 
##            x is created using makeCacheMatrix function. 
## Return: inverse of x

cacheSolve <- function(x, ...) { 
  # get the inverse matrix from cache if available
  inv <- x$getinverse()
  if (!is.null(inv)) {
     message("getting cached data")
     return(inv)
  }
  
  # get the matrix and calculate the inverse value
  data <- x$get()
  inv <- solve(data)
  
  # store the new inverse value
  x$setinverse(inv)
  
  inv
} 
