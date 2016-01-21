## Put comments here that give an overall description of what your
## functions do

## This function creates an object that caches a matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                             ##Nothing is cached so set inv to NULL
        set <- function(y){                                     ##Store the Matrix
                x <<- y
                inv <<- NULL                                    ##The matrix is assigned a new value, flush the cache
        }
        get <- function() x                                     ##Returns the stored Matrix
        setInverse <- function(inverse) inv <<- inverse         ##Cache the given argument
        getInverse <- function() inv                            ##Get cached value
        list(   set = set,                                      ##Return a list where each element of the list is a function
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## This function makes the inverse of the matrix created by makeCacheMatrix above.
## It should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()                   ## Get the cached value
        if (!is.null(inv)) {                    ## If a cached value exists return it
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()                          ## Otherwise get the Matrix and calculate the inverse to store it
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv                                     ## Return the inverse
}
