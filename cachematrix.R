makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL  # Initialize the inverse to NULL
            set <- function(y) {
                x <<- y
                    inv <<- NULL  # Reset the inverse cache when the matrix changes
                      }
                        get <- function() x
                          setInverse <- function(inverse) inv <<- inverse
                            getInverse <- function() inv
                              list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
                              }

}
cacheSolve <- function(x, ...) {
          inv <- x$getInverse()
            if (!is.null(inv)) {
                message("getting cached inverse")
                    return(inv)
                      }
                        data <- x$get()
                          inv <- solve(data, ...)  # Compute the inverse if not cached
                            x$setInverse(inv)
                              inv
                              }
                              
}