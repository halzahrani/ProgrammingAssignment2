## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invmx <- NULL
        set <- function(y) {
                x <<- y
                invmx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmx <<- inverse
        getinverse <- function() invmx
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate the inverse of the special "matrix" 
## If the inverse has already been calculated , then the cachesolve should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmx <- x$getinverse()
        if (!is.null(invmx)) {
                message("getting cached data")
                return(invmx)
        }
        mat <- x$get()
        invmx <- solve(mat, ...)
        x$setinverse(invmx)
        invmx
}
