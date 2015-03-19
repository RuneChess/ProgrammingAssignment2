## The pair of functions below can cache the inverse of a matrix.

## Function 'makeCacheMatrix' creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function 'cacheSolve' computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix' by calling the solve function in R. If the
## inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache. 
## 'cacheSolve' assumes that matrix 'x' is invertible.

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
