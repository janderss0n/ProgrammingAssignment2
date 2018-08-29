## This scrips claculates the inverse of a matrix and
## caches its result.


## makeCacheMatrix creates a special matrix object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve computes the inverse of the matrix object
## returned by makeCacheMatrix. If the inverse has 
## alredy been calculated, then chacheSolve returns
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setInv(inv)
    inv
}

