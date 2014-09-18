## Put comments here that give an overall description of what your
## functions do


#### Write a short comment describing this function
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


#### Write a short comment describing this function
cacheSolve <- function(x, ...) {
	specialVector	<- x
	vectorInverse   <- x$getInverse()
	
	if( is.null(vectorInverse) ) {
		message("calculating inverse")
		specialVector$setInverse()
	} else {
		message("retrieving from cache")
	}
	
	return( specialVector$getInverse() )

}
