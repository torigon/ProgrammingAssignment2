######################################################################
## These functions are to cache the inverse of a matrix.
######################################################################

######################################################################
## This function creates a special "matrix" object 
## that can cache its inverse.
##
## @param x {matrix}    a matrix to be inverse
## @return  {list}  a list contains setter/getter functions
######################################################################
makeCacheMatrix <- function(x = matrix()) {
    ## a cache of 'solve'
    s <- NULL

    ## a function to set a matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    ## a function to get a matrix
    get <- function() {
        x
    }
    
    ## a function to set a 'solve'
    setSolve <- function(solve) {
        s <<- solve
    }

    ## a function to gets a 'solve'
    getSolve <- function() {
        s
    }

    ## returns a list that contains functions
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


######################################################################
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
##
## @param x {function}    a makeCacheMatrix function
## @param ... further arguments passed to a solve function
## @return  {matrix}  the inverse of the matrix
######################################################################
cacheSolve <- function(x, ...) {
    ## sets a 'solve'
    s <- x$getSolve()

    ## if the inverse has been calculated, returns a cached data
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }

    ## gets a matrix
    data <- x$get()

    ## gets the inverse of the matrix 'data'
    s <- solve(data, ...)

    ## sets a 'solve'
    x$setSolve(s)

    ## returns a matrix that is the inverse of 'x'
    s
}
