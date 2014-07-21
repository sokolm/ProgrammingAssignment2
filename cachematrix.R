## The pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
	ix <<- NULL
    }
    get <- function() x
    setinverse <- function(iy) ix <<-iy
    getinverse <- function() ix
    list(set = set, get = get,
         setinverse = setinverse, 
	 getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	ix <- x$getinverse()
	if (!is.null(ix)) {
	    message("getting cashed data")
	    return(ix)
	}
	data <- x$get()
	ix <- solve(data, ...)
	x$setinverse(ix)
	ix
}
