##########################################################################################################################
## makeCacheMatrix: This function takes a matrix and creates an specialized vector which contains a list				##
## of functions. These functions are called in cacheSolve function to return inverse of a matrix from cache.			##
##																														##
## cacheSolve: This function take an specialized vector which has four functions as defined by "makeCacheMatrix"		##
## and returns the inverse of a matrix.																					##
##########################################################################################################################

## makeCacheMatrix: Input	: Not required but if providing an input then it must be a square matrix.
##					Output	: A specialized vector which contains four function to manipulate a square matrix.
##							get, set, getmatrix, setmatrix
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) m <<- inverse
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve:Input		: Specialized list created by makeCacheMatrix.
##			  Output	: Returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
