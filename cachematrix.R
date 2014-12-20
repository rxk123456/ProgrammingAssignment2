## Utility functions to cache the Inverse of a Matrix
## The inverted matrix is calculated the first time it is needed
## and the result is reused for any subsequent requests

## Creates a wrapper around matrix data and creates a placeholder for
## the inverse of the given matrix. The function creates internal functions:
## set - to set the value of the initial matrix data
## get - to get the value of the stored matrix
## setSolution -  to set the value of the inverted matrix
## getSolution - to get the value of the inverted matrix

makeCacheMatrix <- function(matrixData = matrix()) {
    solvedMatrix <- NULL
    set <- function(data) {
        matrixData <<- data
        ## reset any stored solution if the matrix data changes
        solvedMatrix <<- NULL
    }
    get <- function() matrixData
    setSolution <- function(solution) solvedMatrix <<- solution
    getSolution <- function() solvedMatrix
    list(set = set, get = get,
         setSolution = setSolution,
         getSolution = getSolution)
}


## Returns the inverse of the given matrix. It utilizes the functionality
## of the CacheMatrix created by makeCacheMatrix function
## the solution is calculated on the first request and cached value is reused
## for any subsequent requests

cacheSolve <- function(cacheMatrix, ...) {
    ## Return a matrix that is the inverse of data stored in cacheMatrix
    solution <- cacheMatrix$getSolution()
    if(!is.null(solution)) {
        message("getting cached solution")
        return(solution)
    }
    ## if there was no cached solution calculate it and store for future use
    data <- cacheMatrix$get()
    solution <- solve(data, ...)
    cacheMatrix$setSolution(solution)
    solution
}
