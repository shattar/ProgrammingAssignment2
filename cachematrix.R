## This set of functions allows you to create a matrix that is wrapped
## in a mechanism to cache the result of matrix inversion.
## Create a matrix using `makeCacheMatrix`, and retrieve the matrix
## inverse by using `cacheSolve`.

## Wraps a matrix with a mechanism to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Determines the inverse of a matrix.  Uses the cached
## solution it is exists, otherwise calculates the inverse
## and caches the result.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
