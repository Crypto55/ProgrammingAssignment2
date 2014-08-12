## These functions are used to create a special object that stores a matrix 
## and caches its inverse. This only works providing the matrix is invertible.

## The first function makeCacheMatrix creates the special "Matrix" which
## contains functions to set the values of the matrix, get the values of the
## matrix, set the values of the inverse matrix and get the values of the 
## matrixinverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<-y
		m <<-NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<-solve
	getinverse <- function() m
	list (set=set,get=get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## The cacheSolve function calculates the inverse of the special Matrix 
## created with the makeCacheMatrix function. If the inverse has already
## been calculated, it skips the calculation and gets the inverse using 
## the getinverse function. If it hasn't already been calculated, it sets 
## the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
       
}
