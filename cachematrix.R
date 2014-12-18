## Coursera R Programming Language
## Progamming Assignment #2 
## Dec 18, 2014
## ehq

## makeCacheMatrix(): creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(M = matrix()) {

	Minv <- NULL

	set <- function(m) {
		M <<- m
		Minv <<- NULL
	}

	get <- function() { M }

 	setinverse <- function(minv) { Minv <<- minv }

	getinverse <- function() { Minv }

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve(): computes and returns the inverse of the special "matrix" returned by makeCacheMatrix
##    if inverse has already been calculated (and matrix hasn't changed), then 
##    the cached inverse is returned

cacheSolve <- function(M, ...) {

	Minv <- M$getinverse()
  
	# check if inverse has already been calculated (value is not null)
	if (!is.null(Minv)) {
		# if inverse has already been calculated, display message, no need to solve
		message("getting cached data")
	}
	else {
		# calculate the inverse of the matrix
    		Minv  <- solve(M$get())
    		# 'cache' the inverse
    		M$setinverse(Minv)
	}
	return(Minv)  # return the matrix inverse
}
