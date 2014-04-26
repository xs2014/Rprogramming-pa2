# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# a special "matrix" must be a square numeric or complex matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(Solve) m <<- Solve
        getSolve <- function () m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) 
        x$setSolve(m)
        m
}

# For your convenience, you may want to use the following to test my functions:
# m <- matrix(1:4,2)
# cacheSolve(makeCacheMatrix(m))
