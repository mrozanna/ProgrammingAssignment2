## Matrix inversion is usually a costly computation. This is why caching
## the inverse of a matrix rather than coumputing it repeatedly may be beneficial.

## The following two functions first create a special matrix object and then calculate
## the inverse of a matrix (however, if the matrix has not changed and its inverse
## has already been calculated, the inverse will not be computed again but rather will
## be found in the cache and returned).


## The first function makeCacheMatrix creates list containing a function to:
## - set the value of the matrix (function "set")
## - get the value of the matrix (function "get")
## - set the value of the inverse of the matrix (function "setinv")
## - get the value of the inverse of the matrix (function "getinv")

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## The second function cacheSolve computes the inverse of the special matrix returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
