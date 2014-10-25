## Caching the Inverse of a Matrix
## using 2 functions: makeCacheMatrix and cacheSolve


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL

	set <- function(y) {
		x <<- y
		inverse_matrix <- NULL
	}

	get <- function() { x }

	set.inverse.matrix <- function(z) { inverse_matrix <<- z }

	get.inverse.matrix <- function() { inverse_matrix }
	
	list(set = set, get = get,
		set.inverse.matrix = set.inverse.matrix,
		get.inverse.matrix = get.inverse.matrix) 
}


## Calculates the inverse or return the cache result if the cache exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse_matrix <- x$get.inverse.matrix()
	if (!is.null(inverse_matrix)) {
		message("getting cached data")
		return(inverse_matrix)
	}
	
	data <- x$get()
	inverse_matrix <- solve(x,...)
	x$set.inverse.matrix(inverse_matrix)
	inverse_matrix
}
