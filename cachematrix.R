## Hi this is Sinan. Below is my attempt at creating functions that cache the inverse of a matrix

## A function to cache the inverse of a matrix
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}
 

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# set_inv function.
 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("retrieving cached matrix...")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx)
    x$set_inv(inv)
    inv
}
 
