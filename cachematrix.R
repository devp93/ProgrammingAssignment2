###############   makeCacheMatrix   ###############
## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse of the matrix
##  get the value of the inverse of the matrix

###############     cacheSolve      ###############
## cacheSolve calculates the inverse of the special "matrix" created with the above
## function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setsolve function.

## A similar function to the makeVector function, except changing the labels
## from "mean" to "solve" and allowing x to be a matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## A similar function to the cachemean function, except calculating the inverse
## of the input matrix using solve rather than mean. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
