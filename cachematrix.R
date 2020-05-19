## Script for computing inverse of a matrix using a cache function

## This cache function creates a special matrix that stores set, get, setinverse and getinverse functions
## so that second function can be executed without recomputing

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Second function computes the inverse of the matrix by executing the previous function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setinverse(inv)
        return(inv)
}

