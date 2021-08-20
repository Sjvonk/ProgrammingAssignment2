## These functions find the inverse of a matrix, 
## but because calculating the inverse of a matrix
## can be time-consuming, we use the cache to store 
## calculated inverses to save time when calculating 
## the inverse of a matrix that was used before.

## In this function we initiate the matrix and set
## its value and we cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## This function returns the inverse of the inputted matrix x.
## If the inverse was calculated before, it retreives it
## from cache, otherwise the inverse is calculated.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
      	message("getting cached data")
      	return(inv)
	}
	mat <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
