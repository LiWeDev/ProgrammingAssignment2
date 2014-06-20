## Coursera R Programming - Assignment 2
## By Li We
## Last Update 19 June 2014

## Assignment is to write two functions, which are makeCacheMatrix and 
##      cacheSolve, which are listed below.

## makeCacheMatrix: This function creates a special "matrix" object that can 
##      cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## initializing matrix
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## setting get function
    get <- function() x
    
    ## creating inverse function
    setinverse <- function(solve) i <<- solve
    
    ## setting get inverse function
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above. If the inverse has already been 
##      calculated (and the matrix has not changed), then the cachesolve should 
##      retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Getting cached inverse matrix if it is already computed
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    
    ## Otherwise computing inverse of the matrix
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    
}
