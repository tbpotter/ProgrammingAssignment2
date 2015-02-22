## this function creates a cached matrix object. the objects stores the
## matrix and has functions that get/set the matrix values, get/set the matrix
## inverse values and create a list of pairs.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## sets a new value to this matrix. it erases any cached inverse values.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## the get() returns the matrix data
    get <- function() x
    
    ## the setinverse() function "caches" the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    
    ## the getinverse() function returns the value of the matrix inverse
    ## can be null
    getinverse <- function() m
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function will check the matrix object to see if its inverse
## is already cached. If the inverse is cached it will return the result 
## otherwise it will calculate it, cache it and return it.

cacheSolve <- function(x, ...) {
    
    ## get the inverse value from the matrix
    m <- x$getinverse()
    
    ## if it exists (is not null) then return the cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## the inverse does not exist so we need to calculate it
    ## get the matrix
    data <- x$get()
    
    ## calculate the inverse of the matrx
    m <- solve(data, ...)
    
    ## set the inverse value ("cache it")
    x$setinverse(m)
    
    ## return the inverse value
    m
}
