

## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse
## it contains the following functions:
##  setMatrix      set the value of a matrix
##  getMatrix      get the value of a matrix
##  cacheInverse   get the cached value (inverse of the matrix)
##  getInverse     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}
  ## each element of returned list is a function

## cacheSolve: This function computes this inverse of the special "matrix
## returned by makeCacheMatrix above.  If the inverse has already been 
## calculated (and the matrix has not changed) then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
  ## return a matrix that is inverse of "x"