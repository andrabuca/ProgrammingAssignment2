## This is an implementation of a matrix which caches the inverse of the matrix at first


## assumes x is an invertible matrix
## returns an interface for simple setter and getter functions

makeCacheMatrix <- function(A = matrix()) {
  inverse = NULL
  set = function(B) {
    A <<- B
    inverse = NULL
  }
  get = function() A
  setinverse = function(inv) inverse <<- inv
  getinverse = function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of the CacheMatrix as created in makeCacheMatrix
## tries to use cached inverse of the matrix if available

cacheSolve <- function(A, ...) {
  ## a matrix that is the inverse of 'x' is returned
  inv = A$getinverse()
  if (!is.null(inv)) {
    message('getting cached data')
    return(A)
  }
  data = A$get()
  inv = solve(data,...)
  A$setinverse(inv)
  inv
  
}
