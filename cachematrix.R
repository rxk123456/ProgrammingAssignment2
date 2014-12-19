## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrixData = matrix()) {
    solvedMatrix <- NULL
    set <- function(data) {
        matrixData <<- data
        solvedMatrix <<- NULL
    }
    get <- function() matrixData
    setSolution <- function(solution) solvedMatrix <<- solution
    getSolution <- function() solvedMatrix
    list(set = set, get = get,
         setSolution = setSolution,
         getSolution = getSolution)
}


## Write a short comment describing this function

cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- cacheMatrix$getSolution()
    if(!is.null(s)) {
        message("getting cached solution")
        return(s)
    }
    data <- cacheMatrix$get()
    s <- solve(data, ...)
    cacheMatrix$setSolution(s)
    s
}
