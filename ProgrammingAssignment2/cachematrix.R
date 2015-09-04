## A set of functions for inverting large, square matrixes.
## The actual inversion is only done once, then cached. 
## Subsequent invocations uses the cached result.


## makeCacheMatrix caches the matrix, and sets up a list
## of functions to get/set the matrix or its inverse.
makeCacheMatrix <- function(x = matrix()) {
	
	##Null the inverse
	i <- NULL
	
	set <- function(y) {
		##Set a new matrix and null the inverse
		x <<- y
		i <<- NULL
	}
	
	get <- function() {
		##Return the matrix
		x
	}
	
	setinvrse <- function(invrse) { 
		##Cache the inverse
		i <<- invrse
	}
	
	getinvrse <- function() {
		##Return the inverse
		i
	}
	
	##Return the list of set/get functions
	list(set = set, 
		 get = get,
		 setinvrse = setinvrse,
		 getinvrse = getinvrse)
}


## cacheSolve returns the inverse of a square matrix.
## Please note the assumption that x is invertable!
cacheSolve <- function(x, ...) {
    
	##Get and check the cached inverse
	##If not null, return the cached inverse
	i <- x$getinvrse()
	if (!is.null(i)) {
		return(i)
	}
	
	##There was no cached inverse
	##Get the matrix to invert
	data <- x$get()
	
	##Invert the matrix
	i <- solve(data, ...)
	
	##Cache the inverse for the next invocation
	x$setinvrse(i)
	
	##Return the inverse
	i
	
}
