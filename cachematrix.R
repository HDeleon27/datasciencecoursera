## These functions attempt to provide the benefit of caching the inverse of a matrix instead of computing it each and every time it's needed.

## This function creates a list containing a funtion to set and get the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}



## calculates the inverse of the matrix created by the previous function

cacheSolve <- function(x, ...) {
    inv <- x$getI()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
