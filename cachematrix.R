## The two functions are defined to cache the result of inverse of matrix  and check the cached result before processing the inverse of new matrix supplied by the user. If the inverse is already in the cache, it would not compute the inverse again saving processing time.


##Function 'MakeCacheMatrix' holds the value of the original matrix provided by the user and inverse of matrix computed by function ##'cacheSolve'. For the first run, the value is set to NULL.
makeCacheMatrix <- function(x = matrix())  {
	m <- NULL # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
	y <- NULL # sets the value of y to NULL (provides a default if cacheSolve has not yet been used)
		setmatrix <- function(y) { #set the value of the matrix
									x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note #this is within the setmatrix function)
									m <<- NULL ## sets the value of m (the matrix inverse if used cacheSolve) to NULL
								 }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
	    getinverse <- function() m
		list(setmatrix = setmatrix, getmatrix = getmatrix, ## creates a list to house the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function 'cacheSolvechecks' checks if the inverse of the matrix is already been calculated. If it is already calculated then it ##prints the data from the cache. If it is not in the cache, then it takes the new matrix, computes the inverse, sends the new ##inverse to function 'makeCacheMatrix' and returns the inverse to the user.

cacheSolve <- function(x, ...) {
									# Need to compare matrix to what was there before!
									m <- x$getinverse() # if an inverse has already been calculated this gets it
									if(!is.null(m)){ # check to see if cacheSolve has been run before
									message("getting cached data")
									return(m)
													}
	
							# otherwise 
							y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
							x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
							m <- solve(y, ...) # compute the value of the inverse of the input matrix
							x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
							m # return the inverse
}