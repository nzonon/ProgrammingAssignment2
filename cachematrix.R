## This  assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInversematrix <- function(inverse) m <<- inverse
        getInversematrix <- function() m
        list(set = set, get = get,
             setInversematrix = setInversematrix,
             getInversematrix = getInversematrix)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## To compute the inverse of a square matrix can we use the function solve 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mydata <- x$get()
        # 
        m <- solve(mydata, ...)
        x$setInversematrix(m)
        m
}
