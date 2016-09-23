## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute
##      it repeatedly. Thus, you need a pair of functions that cache the inverse of a matrix. 
##      This can be especially helpful when the vectors are large and when the data is static.

## This function creates a special "matrix" object that can cache its inverse. It is really a list containing a function to
##      set the value of the vector
##      get the value of the vector
##      set the value of the mean
##      get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <- y
    invrs <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <- inverse
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the mean of the special "vector" created with the makeCacheMatrix function above. 
##      However, it first checks to see if the mean has already been calculated. 
##      If so, it gets the mean from the cache and skips the computation. 
##      Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat, ...)
  x$setinverse(invrs)
  invrs ## Return a matrix that is the inverse of 'x'
}
