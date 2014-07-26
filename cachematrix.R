## The purpose of these 2 functions is to create a cached memory area
## where a computed value for a square matrix inverse can be stored for
## retrieval rather than being re-calculated when needed for use.

## The makeCacheMatrix function creates a section of memory that
## can store a matrix of the same size that is passed to it. It
## also cretes methods to get and set the value in the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## define the methods of the function used to get 
        ## and set the cached value.
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


## The cacheSolve function passes the inverse of a square matrix as
## the result of calling the function. The function checks if the 
## inverse has already been calculated and stored in memory. If the
## inverse has already been calucalted then it will be returned from the 
## cache, otherwise the inverse is calculated, stored in memory and returned
## as the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## by calling the get method of the cache with 
        ## the data passed to the function
        m <- x$getsolve()
        ## if the value is in cache then return the cached value.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if the inverse is not stored in cache then compute
        ## the inverse and call the set method to store the 
        ## inverse value in the cached memory
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
