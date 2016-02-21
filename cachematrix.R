# Coursera R Language Programming.  
#	Programming Assignment 2: Lexical Scoping
# 	Submitted by: Gustavo Zarrate
## Put comments here that give an overall description of what your
## functions do
# These two functions cache the inverse of a matrix.

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" that is a list containing a function to:
#	set the value of the matrix
#	get the value of the matrix
#	set the value of the inverse
#	get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	locMatrix <- NULL
	
	set <- function(aMatrix)
	{
		x <<- aMatrix
		locMatrix <<- NULL
	}
	
	get <- function() x
	setinv <- function(inv) locMatrix <<- inv 
	getinv <- function() locMatrix
	
	list(set = set, get = get, 
		setinv = setinv, 
		getinv = getinv)

}

## Write a short comment describing this function
# cacheSolve calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
# via the setinv function.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
		
	locMatrix <- x$getinv()

	# Check if inverse is NOT null.
	if (!is.null(locMatrix)) {
		message("Returning cached inverse...")
	}
	else {
		message('Computing inverse...')
		data <- x$get()
		locMatrix <- solve(data, ...)
		x$setinv(locMatrix)
	}
	
	return(locMatrix)		
	
}

