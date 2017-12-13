## Utility functions to calculate the inverse of a Matrix.
## Specifically, for a given CacheMatrix object, the result
## will be cached providing the underlying data is not modified.
## Example:
##  > m <- makeCacheMatrix(matrix(1:4,2,2))
##  > cacheSolve(m)
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1


## Create the CacheMatrix object.
## Functions: set, get, setinverse, getinverse
##
## NOTE: assumes the matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Given a CacheMatrix object created with 'makeCacheMatrix'
## calculate the inverse and cache the result using the solve() function.
##
## Additional args can be passed to solve via the `...` parameters
## However, these are ignored if there is already a cached solution for
## the given object.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
