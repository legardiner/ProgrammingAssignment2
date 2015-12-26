## Calculating the Inverse of a Matrix Using Lexical Scoping
## Matrix inversion is usually a costly computation and there may be some 
## benefit to using lexical scoping to cache the inverse of a matrix rather 
## than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## The "makeCacheMatrix" function creates a special "matrix" object. It is
## a list containing a function to set the value of the matrix, get the value
## of the matrix, set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The "cacheSolve" function calculates the inverse of the special "matrix" object
## created with the "makeCacheMatrix" function. However, it first checks to see if the
## inverse has already been calculated. If so, it retrieves or "gets" the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the "setInverse"
## function.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
