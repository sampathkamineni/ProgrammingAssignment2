## The following is a pair of functions that cache and compute the
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## initialize the cache matrix 'cacheMatrix'
	cacheMatrix <- NULL
	
	## define the method 'setMatrix'
	setMatrix <- function(y) {
	x <<- y
	cacheMatrix <<- NULL
	}
	
	## define the method 'getMatrix' and return the matrix 'x'
	getMatrix <- function()x

	## define the method named 'setCache'
	setCache <- function(inverse) cacheMatrix <<- inverse

	## define the method named 'getCache'
	## that will return the cached inverse of 'x'
	getCache <- function() cacheMatrix

	## list the names of all the methods
	list(setMatrix = setMatrix, 
	getMatrix = getMatrix, 
	setCache = setCache,
	getCache = getCache)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
	## check the content of cache matrix
	cacheMatrix <- x$getCache()
	
	## if the content is not null, return the result
	if (!is.null(cacheMatrix)) {
		message("Getting cached data...")
		return(cacheMatrix)
	}

	## if the content is empty, get the matrix
	## create, set, update, and return the cache matrix
	else {
		dMatrix <- x$getMatrix()
		cacheMatrix <- solve(dMatrix, ...)
		x$setCache(cacheMatrix)
		return(cacheMatrix)
	}

## Return a matrix that is the inverse of 'x'
}
