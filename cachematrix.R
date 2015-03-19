## The following is a pair of functions that cache 
## the inverse of a matrix. (Assignment week 3 - R Programming)


## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## A list containing a function to
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
  
  inv = NULL
  set = function(y) { 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("Getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
