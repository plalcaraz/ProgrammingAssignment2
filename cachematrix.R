## Two functions: makeCacheMatrix and cacheSolve. The objective is to calculate
## the inverse of a matrix, store its result in order to consult its value instead of 
## calculating it again if needed
## 
##

## makeCacheMatrix: input a matrix, output list a functions:
## set and get of the matrix
## set and get of its inverse
## NOTE: actually setsolve can be used to store any data, the functions do not perform a
## check for the inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve: input the list returned by makeCacheMatrix
## output the inverse of the matrix store in the list
## If the list does not contains the inverse, the inverse is calculated, else the inverse is
## directly returned from stored value in the list and no calculation is needed.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,)
        x$setsolve(s,...)
        s
}
