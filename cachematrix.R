## As matrix inversion is a costly computation, these functions allow for
## the caching of the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # note that x needs to be input as an invertable matrix
    
    s <- NULL
    # sets variables to cache
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    # note that solve is the function that inverts a matrix
    setsolve <- function(solve) s <<- solve  #sets inverted matrix to cache
    getsolve <- function() s
    
    #create a list of functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    # note that x needs to be an invertable matrix
    s <- x$getsolve()
    
    # first checks to see if inverted matrix exists in cache, 
    # if it does exist in cache it returns the value it finds there
    if(!is.null(s)) {
       message("getting cached data")
       return(s)
    }
  
    data <- x$get()
    s <- solve(data, ...) #inverts the matrix
    x$setsolve(s)
    s    ## Returns a matrix that is the inverse of 'x'
}
