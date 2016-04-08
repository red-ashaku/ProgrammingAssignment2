## Programming in R
## Assignment #2: Caching a Matrix
## This program caches an inverted matrix for rapid access

## This function creates a "matrix" object, which is a list of functions that carry out matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    return(list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve))
}


## This function reads in the "matrix" object, checks if an inverted matrix exists,
## outputs the inverted matrix if yes, inverts and outputs the result if no

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    return(s)
}
