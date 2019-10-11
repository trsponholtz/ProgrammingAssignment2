## The following functions are designed to more efficiently generate the
## inverse of a given matrix in situations where it must be caluclated
## repeatedly by storing the inverse along with the original matrix.

## The makeCacheMatrix function takes a matrix as an argument and creates
## an object which will store the inverse of the matrix once it has been
## calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve fucntion takes an object created by makeCacheMatrix
## as an argument. If the inverse of the matrix has previously been
## calculated, it returns the cached inverse. Otherwise, it calculates
## the inverse of the matrix, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
