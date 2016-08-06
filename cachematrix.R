## 
## 

## This function creates a special "matrix" object that can cache 
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list (set = set, get = get,
		setInv = setInv,
		getInv = getInv)
}


## This function checks whether the inverse of the special "matrix" object created
## by the makeCacheMatrix function exists. If it does exist, the cached inverse matrix 
## is returned. If it does not exist, the inverse of the matrix is computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'	
	inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat.data <- x$get()
	inv <- solve(mat.data, ...)
	x$setInv(inv)
	inv
}
