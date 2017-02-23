## These functions are for caching the inverse of a matrix
## cause matrix inversion is a time-consuming computations.
## If the inverse has already been calculated and the matrix
## has not changed, then the cached inverse will be offered.  

## This function makeCacheMatrix creates a list of functions:  
## 1.$set to set the matrix to invert
## 2.$get to get the matrix to invert
## 3.$setinverse to cache the inverse of the matrix
## 4 $getinverse to get that cache of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        if(class(try(solve(y), silent = T)) != "matrix") {
            warning("matrix is not invertible")
            return(y)
        }
        if(identical(y,x)) {
            warning("Same matrix! Nothing will be changed ")
            return()
        }
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function cacheSolve computes the inverse of the matrix 
## returned by makeCacheMatrix and cache it if the matrix has 
## changed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i) && !is.na(i)){
        message("getting cached inverse of matrix")
        return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$setinverse(i)
    i
}
