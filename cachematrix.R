## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # inv will store the cached inverse matrix
        inv <- NULL
        
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Getter for the matrix
        get <- function() x
        
        # Setter for the inverse
        setinverse <- function(inverse) inv <<- inverse
        # Getter for the inverse
        getinverse <- function() inv
        
        # Return the matrix with our newly defined functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        # If the inverse is already calculated, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # The inverse is not yet calculated, so we calculate it
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setinverse(inv)
        
        # Return inverse
        inv
}
