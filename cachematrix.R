## > Put comments here that give an overall description of what your
## > functions do

## makeCacheMatrix creates a "matrix object" with set/get functions
## and setinv/getinv functions
## Actual calculation (and caching) of inverse is done in cacheSolve
## Example usage:
##   m <- makeCacheMatrix(matrix(rnorm(10000), 100, 100))
##   cacheSolve(m) # this solves and caches inverse
##   cacheSolve(m) # and just spits out cached version

## > Write a short comment describing this function
## So, this is just like makeVector: setter sets x and unsets i
## (which is cached inverse)

makeCacheMatrix <- function(x = matrix()) {
    # i contains NULL or cached inverse
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i

    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## > Write a short comment describing this function
## check if there's and inverse cached, calculate and cache if not and return it
cacheSolve <- function(x, ...) {
    # retrieve cached version
    i <- x$getinv()
    if(!is.null(i)) {
        # and return it if it's there
        message("getting cached data")
        return(i)
    }

    data <- x$get()
    # solve, cache and return solution
    i <- solve(data, ...)
    x$setinv(i)
    i
}
