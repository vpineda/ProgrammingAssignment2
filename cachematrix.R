## Functions will compute and cache the inverse of a matrix
## functions do

## makeCacheMatrix will create a matrix object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL        	## inverse which is reset to NULL
        set <- function(y) {
                ## takes and saves the input matrix then resets inv to NULL
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        ## stores a value using superassignment to be accessed later
        getinverse <- function() inv
        ## returns cached value when applicable
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## list of internal functions for the calling function to follow
}

## cacheSolve will compute the inverse of the matrix returned by
## makeCacheMatrix. If the inverse of the returned matrix has
## already been calcuated, cacheSolve will retrive the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {   
                ## checks if inverse has already been calcualted
                message("getting cached data")
                return(inv)
                ## if already calculated retrieves and returns saved inverse
        }
        data <- x$get()
        inv <- solve(data, ...)		## calculates matrix inverse
        x$setinverse(inv)
        inv          ## returns matrix inverse
}
