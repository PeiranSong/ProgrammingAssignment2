## Assignment 2: Caching the inverse of a matrix

## Below are two functions that are used to create a special object 
## that stores an invertable matrix and caches its inverse.



## This function creates a special "matrix" object 
## which is a list containing functions to set and get the value of
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        
        inverse
}
