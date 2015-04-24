## This functions can be used to reduce the memory used to calculate the inverse of a matrix


## makeCacheMatrix creates a special matrix object used to cache its inverse
makeCacheMatrix <- function(x = matrix(), n = nrow(x)) {
	x <- matrix(x, ncol = n, nrow = n)
	inv <- NULL
	setmatrix <- function(y = matrix(), m) { ## this function is used to replace the original input x by a new input y, in the main function
		y <- matrix(y, ncol = m, nrow = m)
		x <<- y
		inv <<- NULL
	}
	getmatrix <- function() x 
	setinv <- function(solve) inv <<- solve ## stores the value of the current input in inv
	getinv <- function() inv  
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinv = setinv, getinv = getinv)
}


## This function solves the inverse of the matrix from the function makeCacheMatrix, using cached data, if available

cacheSolve <- function(x, ...) {
	inv <- x$getinv() 
		if(!is.null(inv)){ ## verify previous stored data in cache
			message("getting cached data")
			return(inv)
		}
		else{ 
			data <- x$getmatrix()
			inv <- solve(data, ...)
			x$setinv(inv)
			inv
		}
}