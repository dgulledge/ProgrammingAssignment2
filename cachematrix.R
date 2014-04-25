## cacheMatrix.R contains an implementation of "memoization" of the
## matrix inverse function.  (See http://en.wikipedia.org/wiki/Memoization).

## This function creates an object containing a matrix that can
## cache the inverse of that matrix the first time it is calculated.
## The avoids costly recalculation when the result has already been
## calculated.
## It relies on a closure to encapsulate the stored matrix and its
## inverse.  These are stored in the variables x and i respectively.
## It returns a list containing the functions that use the data from
## the closure.  Those functions are:
##
##   set(y)    Set the stored matrix to a new value y.  Reset the stored
##             inverse so that it will have to be recalculated.
##   get()     Get the stored matrix.
##   setinverse(inverse)
##             Used internally by cacheSolve() to set store the inverse
##             when it is calculated.
##   getinverse()
##             Get the stored inverse.  If it has not yet been calculated
##             it will have the value NULL.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) i <<- inverse

    getinverse <- function() i

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This is a "memoized" version of the solve() function.  It requires that
## the matrix it operates on has been encapsulated by the makeCacheMatrix()
## function.  This provides cached storage for the matrix itself and its
## inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()

    if (!is.null(i)) {
        # If a cached copy of the answer exists, use it.
        message("getting cached data")
    } else {
        # Get the stored data.
        data <- x$get()

        # Calculate the inverse.
        i <- solve(data)

        # Cache the inverse for future use.
        x$setinverse(i)
    }

    # Return the answer
    i
}
