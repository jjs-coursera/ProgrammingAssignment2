#### Return a "cacheMatrix", which is a special list of functions
#### that define and retrieve a matrix and its inverse
####
#### Due to lexical scoping, the matrix and inverse are stored
#### locally to the function, like an object.

makeCacheMatrix <- function(x = matrix()) {
	myMatrix 	<- x
	myInverse	<- NULL

	return(list(
		set = function(y) {
			myMatrix  <<- y
			myInverse <<- NULL
		},
		
		get = function() {
			return(myMatrix)
		},
		
		setInverse = function() {
			myInverse <<- solve(myMatrix)
		},
		
		getInverse = function() {
			return(myInverse)
		}
	))
		
}


#### Given a "cacheMatrix", return the inverse from its cache.
#### If the cache value is undefined, call the function to define it first.

cacheSolve <- function(x, ...) {
	cacheMatrix		<- x
	matrixInverse   <- x$getInverse()
	
	if( is.null(matrixInverse) ) {
		message("calculating inverse")
		cacheMatrix$setInverse()
	} else {
		message("retrieving from cache")
	}
	
	return( cacheMatrix$getInverse() )

}
