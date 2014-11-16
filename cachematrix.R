## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
## In this file, we have a pair of functions that cache the inverse of a matrix.


## The function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to set and get the value and inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv_m) inv_matrix <<- inv_m
        getinverse <- function() inv_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function, cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setinverse(inv_matrix)
        inv_matrix
}
