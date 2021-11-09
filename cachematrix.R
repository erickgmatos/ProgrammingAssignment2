## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix(x) creates a cache matrix with matrix x that can
## stores the inverse of the matrix x
## The cache matrix created posses the object methods set(), get(), 
## setinverse() and getinverse()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x), with x being an object of the type makeCacheMatrix(), verify
## if there is a inverse stored in the matrix cache. If not, it calculates its
## inverse, or else it retrieve the value of the inverse matrix stored 
## in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
