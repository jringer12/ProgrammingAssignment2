## makeCacheMatrix and cacheSolve can be use to create an object
## that can 'cache'/return the inverse of a matrix 

## makeCacheMatrix: Creates matrix-type object that contains a list
## of function to set/get a matrix and its inverse. Input is a valid,
## invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) x_inverse <<- solve
        getinverse <- function() x_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of a matrix by first checking to see if,
## the inverse is already in memory. The input must a matrix of type
## makeCacheMatirx.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getinverse()
                if(!is.null(x_inverse)) {
                        message("getting cached data")
                        return(x_inverse)
                }
                data <- x$get()
                x_inverse <- solve(data, ...)
                x$setinverse(x_inverse)
                x_inverse
}
