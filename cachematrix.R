makeCacheMatrix <- function(x = matrix()) {
        m <- NULL       ## The variable m will be set at NULL and will store our inverse.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                m <<- inverse
        }
        getinverse <- function() {
                m
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function will make a cache for the matrix and its inverse. Then, list them as objects

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                ## If the inverse has been calculated, this will be retrieved from the cache.
                ## Otherwise, a calculation for the new matrix will be made.
        }
        data <- x$get()
        m <- solve(data, ...) ##The solve function will get the inverse of a matrix.
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
##This function will solve for the inverse of a matrix and store it in the cache.
