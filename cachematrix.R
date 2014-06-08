## These functions decorate a matrix with a memo that caches the value of
## the matrix inverse. After the inverse is calculated once, subsequent calls
## to getInverse will retrieve the cached version. If the matrix is modified,
## the cache is reset.

## Sample usage:
##     myMatrix = matrix(rnorm(9), nrow=3, ncol=3)
##     myCachedMatrix = makeCacheMatrix(myMatrix)
##     inverse = cacheSolve(myCachedMatrix)    # inverse is calculated
##     x = myCachedMatrix$get()                # retrive the value of the matrix
##     inverse2 = cacheSolve(myCachedMatrix)   # inverse is not recalculated, previous value is returned
##     myCachedMatrix$set(anotherMatrix)       # set the matrix to a new value
##     inverse3 = cacheSolve(myCachedMatrix)   # inverse of the new matrix is calculated

## makeCacheMatrix: Attach a matrix inverse cache to a given matrix.
##
## Calling myCachedMatrix <- makeCacheMatrix(myMatrix) returns a list of 4 functions:
##    myCachedMatrix$get() returns the value of the matrix
##    myCachedMatrix$set(y) will set the value of the matrix to a new value y
## The other two functions are intended for internal use only:
##    myCachedMatrix$getInverse() will return the cached inverse of the matrix, if it exists.
##    myCachedMatrix$setInverse(inverse) will cache the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, 
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## cacheSolve : return the inverse of a cached matrix
##
## This function returns the inverse of an augmented matrix created by makeCacheMatrix.
## The first time this is called after creation (or modifying the matrix value), the inverse
## will be calculated and cached for later retrieval. If cacheSolve is called again and the
## matrix has not been modified, the cached value will be returned and the inverse will not
## be recalculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
