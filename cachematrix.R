## This is my humble solution of the second assignment
## hope this is good enough - it is very similar to the examples
##**********************************************************************

## the first function to cache the inverse - Returns four functions
makeCacheMatrix <- function(x = matrix()) {

        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setI <- function(invert) I <<- invert    ## will be used in the 2nd function
        getI <- function() I                     ## will be used in the 2nd function
        list(set = set, get = get,
             setI = setI,
             getI = getI)
}

##*******************************************************************************

## the secong function to compute the inverse only if it is not calculated before
cacheSolve <- function(x, ...) {
                                      ## Return a matrix that is the inverse of 'x'
        I <- x$getI()
        if(!is.null(I)) {
                                      ## this is the case when the calculations
                                      ## has been made before
                
                message("getting cached data")
                 return(I)
        }
        data <- x$get()
        I <- solve(data, ...)         ## the actual calculation of inverse
        x$setI(I)
        I
}
