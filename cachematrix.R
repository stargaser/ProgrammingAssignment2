## The functions in this file allow caching of
## the inverse of a matrix, to potentially save time
## since the inverse of a matrix can be costly to compute

## makeCacheMatrix creates a special matrix that is really
## a list containing functions to set and get the value of
## the matrix, and to set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL ## clear cache because the values are new
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    }


## cacheSolve is a function that checks if the
## inverse is cached. If yes, return the cached value.
## If no, compute and return the inverse while caching it too.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
