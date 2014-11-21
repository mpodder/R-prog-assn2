#Assignment: Caching the Inverse of a Matrix

#1. makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function( d = matrix() ) {

    # Initialize the inverse property
    i <- NULL

    # Method to set the matrix
    set <- function( matrix ) {
            d <<- matrix
            i <<- NULL
    }

    # Method to get the matrix
    get <- function() {
    	# Return the matrix
    	d
    }

    # Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    # Method to get the inverse of the matrix
    getInverse <- function() {
        # Return the inverse property
        i
    }

    # Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#2. cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {

    # Setting a matrix that is the inverse of 'x'
    i <- x$getInverse()

    # Just return the inverse if it is already set
    if( !is.null(i) ) {
            message("getting cached inverse")
            return(i)
    }

    # Get the matrix from our object
    message("calculating the inverse from the cached data/matrix")
    data <- x$get()

    # Calculate the inverse using matrix multiplication
    i <- solve(data) %*% data

    # Set the inverse to the object
    x$setInverse(i)

    # Return the inverse of the matrix
    i
}
