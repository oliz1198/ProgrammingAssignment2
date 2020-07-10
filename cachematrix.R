## To cache the inverse of a matrix so that it does not have to be 
## computed repeatedly

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setin <- function(solve) i <<- solve
    getin <- function() i
    list(set = set, get = get,
         setin = setin, 
         getin = getin)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getin()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setin(i)
    i
}
