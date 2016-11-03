# These functions are used to cache the inverse of a matrix.
# How to Use:
# > m <- matrix(c(1, 0, 0, 2), nrow=2)
# > mcm <- makeCacheMatrix(m)
# > mcm$get()
# [,1] [,2]
# [1,]    1    0
# [2,]    0    2
# > cacheSolve(mcm)
# [,1] [,2]
# [1,]    1  0.0
# [2,]    0  0.5
# > cacheSolve(mcm)
# getting cached data
# [,1] [,2]
# [1,]    1  0.0
# [2,]    0  0.5

makeCacheMatrix <- function(mat = matrix()) {
    # Creates a special "matrix", which is a list containing a function to
    # 1.set the value of the matrix
    # 2.get the value of the matrix
    # 3.set the value of the inverse of the matrix
    # 4.get the value of the inverse of the matrix
    #
    # Args:
    #   mat:      A matrix.
    # Returns:
    #   A special "matrix".

    # Prepare space to cache the inverse of the matrix.
    cachedInverse <- NULL

    set <- function(m) {
        # This is a function to set a matrix.
        #
        # Args:
        #   m:  A matrix.
        # Returns:
        #   None.
        mat <<- m
        cachedInverse <<- NULL
    }

    get <- function() {
        # This is a function to get a matrix.
        #
        # Returns:
        #   A matrix.
        mat
    }
    
    setInverse <- function(inv) {
        # This is a function to set the inverse of the matrix.
        #
        # Args:
        #   inv:  The inverse of the matrix.
        # Returns:
        #   None.
        cachedInverse <<- inv
    }

    getInverse <- function() {
        # This is a function to get the inverse of the matrix.
        #
        # Returns:
        #   The inverse of the matrix.
        cachedInverse
    }

    # Returns a list that contains functions.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(cmat, ...) {
    # Computes the inverse of the special "matrix".
    #
    # Args:
    #   x:      A special "matrix".
    #   ...:    Remaining arguments passed to the function solve().
    # Returns:
    #   The inverse of the special "matrix".

    cachedInverse <- cmat$getInverse()

    # If the inverse has been calculated, returns a cached data
    if (!is.null(cachedInverse)) {
        message("getting cached data")
        return(cachedInverse)
    }

    m <- cmat$get()

    ## Computes the inverse of the matrix.
    cachedInverse <- solve(m, ...)

    cmat$setInverse(cachedInverse)

    cachedInverse
}
