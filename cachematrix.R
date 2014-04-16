## The makeCacheMatrix and cacheSolve function are optimized to
## return an inverted matrix that is passed through the function.
## This is done by caching the calculations in the global environment
## in a variable that can be called later and not need to be recomputed. 

## The makeCacheMatrix function calculates and caches mean computations
## from the matrix passed through the function. 

makeCacheMatrix <- function(x = matrix()) {
    
    # Create a NULL variable as an accumulator
    cache <- NULL
    #Function to cache the calculation
    set <- function(x) {
        y <<- matrix
        cache <<- NULL
    }
    # Function to get the desired mean computations
    get <- function() x
    setmean <- function(mean) cache <<- mean
    getmean <- function() cache
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)    
}

## The cacheSolve function utalizes the chached
## mean computations calculated by the makeCacheMatrix
## and returns an inverted matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mymatrix <- solve(x, ...)
    
    # Get previously calculated mean
    # if it has been cached
    cache <- mymatrix$getmean()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    
    data <- mymatrix$get()
    cache <- mean(data, ...)
    mymatrix$setmean(cache)
    return(mymatrix)
}
