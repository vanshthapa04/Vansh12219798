# makeCacheMatrix: Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve: Computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
