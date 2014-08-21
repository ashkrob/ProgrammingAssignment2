# Below are two functions that are used to create a special object
# that stores a matrix and cache's its inverse.


# The first function, makeCacheMatrix creates a special "vector",
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


# The second function cacheSolve calculates the inverse of the special "matrix"
# created and returned by makeCacheMatrix function.
# However, if the inverse has already been calculated, it gets the inverse
# from the cache and skips the calculation.
# And if the cache is empty, it calculates the inverse of the data and sets the value
# of the inverse in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
