## These functions can be used to find the inverse of a matrix either by retrieving it from the cache,
## if it is stored there. or by calculating it. This is done by first converting the matrix into a special
## vector and then apply a function to find the inverse matrix.


## Function takes matrix (invertible) as input.
## Outputs list with four functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(nondeg) inv <<- nondeg
        getinverse <- function() inv
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Function retrieves inverse matrix if stored in cache.
## Otherwise it calculates the inverse matrix and sets it in the cache.

cacheSolve <- function(x, ...) {
        vers <- x$getinverse()
        if(!is.null(vers)) {
            message("Getting cached data...")
            return(vers)
        }
        inmat <- x$get()
        vers <- solve(inmat)
        x$setinverse(vers)
        vers
}
