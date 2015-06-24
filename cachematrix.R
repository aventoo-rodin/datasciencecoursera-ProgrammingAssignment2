# This function creates a special object to cache matrix/inverted matrix pair
makeCacheMatrix <- function(x = matrix()) {
    # That's where cached inverted matrix is stored
    inv <- NULL
    # This function replaces original matrix and resets cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Returns original matrix
    get <- function() x
    # Sets inverted matrix
    setinv <- function(inverse) inv <<- inverse
    # Returns inverted matrix
    getinv <- function() inv
    # This is a list of 4 functions, which operate with cached inverted matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# This function find inverse matrix and uses cache if possible
cacheSolve <- function(x, ...) {
    # Check cache first
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("We've got cached version")
        return(inv)
    }
    # Calculate inverse matrix
    m <- x$get()
    inv <- solve(m)
    # Save inverse matrix in the cache
    x$setinv(inv)
    inv
}
