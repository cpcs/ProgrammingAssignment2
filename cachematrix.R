##  write a pair of functions that
##  cache the inverse of a matrix.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    ## solve is a function to calculate the inverse of a matrix
    setinverse <- function(solve) I <<- solve
    getinverse <- function() I
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
            message("getting cached inverse matrix data")
            return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I

}

