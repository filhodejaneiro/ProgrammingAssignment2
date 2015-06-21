## These two functions work together, helping to cache
## the result of potentially time-consuming inverse of a matrix

## This funcion returns a helper object, which is a matrix plus cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # cache for the inverse
    set <- function(y) { ## setter for the argument matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## getter for the argument matrix
    setinverse <- function(inverse) inv <<- inverse ## setter for the cache
    getinverse <- function() inv ## getter for the cache
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function returns the inverse of a matrix stored in a helper object
## as returned by the makeCacheMatrix() function, retrieving it from the cache if
## the cached value exists

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
