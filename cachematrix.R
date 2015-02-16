##  Srivatsa Gorur
##  Coursera R Programming Week 2 Exercise
##  Feb 16, 2015

## The set of functions below collectively perform Caching
## the inverse of a Matrix. Matrix Inverse is a costly
## operation and hence caching provides benefit while
## computing an inverse repeatatively

## makeCacheMatrix function creates a special 'matrix' object
##  that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse to NULL
    m <- NULL
    
    ## to set the data matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## to get the data matrix
    get <- function() x
    
    ## to set the inverse matrix
    setinverse <- function(solve) m <<- solve
    
    ## to get the data matrix
    getinverse <- function() m
    
    ## list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix object returned
## by the above function if it is not already computed, else 
## it returns the inverse from the cache

cacheSolve <- function(x, ...) { 
    
    ## First look at the cache
    ## and return if not NULL
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise get the data matrix
    ## and compute the inverse using 'solve'
    ## function
    data <- x$get()
    m <- solve(data, ...)
    
    ## set the inverse back to the cache
    x$setinverse(m)

    ## Return m matrix that is the inverse of 'x'
    m
}
