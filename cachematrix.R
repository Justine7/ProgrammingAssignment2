## Set of two functions to create a special matrix
## whose inverse can be cached

## makeCacheMatrix
## The input is an invertible matrix 
## This function creates the structure
## of the special matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	# set : creates a new matrix and force the inverse to NULL value
    set <- function(y) {
		x <<- y
		inv <<- NULL
    }
	
	# get : returns the matrix value
	get <- function() x
	
	# setinv : set that the inv value is the inverse of the matrix
	setinv <- function(solve) inv <<- solve
	
	# getinv : returns the inverse of the matrix
	getinv <- function() inv
	
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)

}


## cacheSolve
## inputs :	x, a matrix created with makeCacheMatrix
##			... options for the function solve
## output : the inverse of the matrix x, 
##			either taken from the cache if already computed
##			or computed using solve

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()		# get the cache value
	if (!is.null(inv)) {	# test if the inverse was cached
		message("getting cached data")
		return(inv)
	}
	data <- x$get()			# get the matrix itself
	inv <- solve(data, ...) # compute the inverse
	x$setinv(inv)			# cache the computed inverse
	inv
}
