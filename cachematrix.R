## Put comments here that give an overall description of what your
## functions do

## This function is used to make a wrapper for a given matrix, adding getters and setters
## for the matrix and its inverse separately

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This funciton is used to get the inverse of a matrix wrapped in the 
## object returned by makeCacheMatrix function. If the given matrix's inverse
## is already cached, then the cached result is returned; otherwise, the inverse
## is computed and cached, then return the result

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) {
      message("getting cached data")
        return(s)
    }
    data <- x$get()
    im <- diag(nrow(data))
    s <- solve(data,im)
    x$setSolve(s)
    s
}
