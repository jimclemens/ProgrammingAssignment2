## These two functions reduce computational load by 
## providing a facility to recognize that a matrix
## inverse has already been calculated and to return the
## cached solution instead of computing each time it
## is needed.

makeCacheMatrix <- function(x = matrix()) {

	## makeCacheMatrix sets up four functions that are used 
	## to store and get the source matrix and the inverse.
		
	## The variable inv holds the value of the solution, or NULL
	## if the solution has not been computed for the current value
	## of the matrix.

	inv <- NULL
	
	## the set function can be used to change the source matrix
	## values. inv must be set to NULL if the source matrix is
	## changed.
	
	set <- function(y = matrix()) {
		x <<- y			## store the matrix in the parent environment
		inv <<- NULL	## indicates no solution computed yet
	}
	
	## get retrieves the value of the matrix; setinv stores the value
	## of the inverse; getinv retrieves the value of the inverse.
	get <- function() x
	setinv <- function(soln) inv <<- soln
	getinv <- function() inv
	
	## return a list of the functions
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x) {

	## When invoked after using makeCacheMatrix, cacheSolve
	## returns the cached value of the inverse of the matrix
	## or computes the inverse (if there is no cached value),
	## stores the result in the variable inv and returns it.

	inv <- x$getinv()		## retrieve the inverse or NULL
	if (!is.null(inv)) {
		message("getting cached solution")
		return(inv)			## return the cached inverse
	}
	
	## If no inverse cached, compute it and store it using setinv
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv						## return the calculated inverse
}
