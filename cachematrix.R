## The two functions below are used to illustrate scoping rules
## as changes are applied to variables 'x' and 'inv'


## This function creates a special list object that assigns
## values and functions to several variables

makeCacheMatrix <- function(x = matrix()) {
	# x is defined in arg above, and inv is nulled out
	inv <- NULL

	# function definition alters parent variables
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# get the value of matrix
	get <- function() x

	# cache value of the inverse
	setinv <- function(inverse) {
		inv <<- inverse
	}

	# gets value of inverse
	getinv <- function() inv  
	
	# return list that contains all the values and functions
	list(set = set, get = get, setinv = setinv, 
		getinv = getinv)
}


## This function computes (if necessary) and returns the matrix inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	
	# check if cache is stored, return inv if it's there
	if (!is.null(inv)) {
		message("getting cached data...")
		return(inv)
	}

	# assign value of matrix using the list object
	matrix <- x$get()

	# compute matrix inverse
	inv <- solve(matrix)

	# cache value of inverse
	x$setinv(inv)

	# return inverse
	inv
}

