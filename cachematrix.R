## Put comments here that give an overall description of what your
## functions do

## Makes an object which holds both a matrix and a spot to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# Sets the value of the matrix and resets the cached inverse.
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	# A function to retrieve the stored matrix.
	get <- function() x
	# Functions to store and retrieve the cached inverse.
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse

	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function takes an object created using 'makeCacheMatrix()' and
#  returns its inverse by either calculation or retrieving the cached answer.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	# Retrieve the cached inverse.
	inv <- x$getinverse()
	if(!is.null(inv)) {
		# If the inverse is already calculated, return it.
		#message("getting cached data")
		return(inv)
	}
	# If the cached inverse is null, calculate it and store it in the
	# cache.
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
