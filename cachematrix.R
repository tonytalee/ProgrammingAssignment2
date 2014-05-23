## These two funcions are used to create a special object that stores a numerical matrix 
## and caches its inverse.



## makeCacheMatrix creates a special vector which is a list containing a function to 
## (1) set value of the vector; (2) get the value of vector; (3) set the value of the
## inverse; (4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the mean of the special "vector" created with makeCacheMatrix.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the mean in the cache via
## the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
