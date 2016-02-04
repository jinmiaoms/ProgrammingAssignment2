## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly 

## This function reates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    imatrix <- NULL
    set <- function(m) {
        x <<- m
        imatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) imatrix <<- inv
    getInverse <- function() imatrix
    list(set = set, get = get,
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getInverse()
    if (!is.null(im)) {
        message("Get data from cache")
        return(im)
    }
    m <- x$get()
    im <- solve(m)
    x$setInverse(im)
    im
}

