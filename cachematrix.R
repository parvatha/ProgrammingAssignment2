## Purpose: Compute / Retrieve matrix inversion (from cache)
##
## Assumption: Matrix supplied is always invertible
##
## Description: Matrix inversion is usually a costly computation and following pair of functions are used to
##              create a matrix and compute / retrive (from cache if calcualted ) inverse of the matrix 
##
## 1. makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
##
## Description: Set / Get the value of the matrix and inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
##
## 2. cacheSolve: Computes the inverse of the special "matrix" returned by above makeCacheMatrix function.
##                Retrieves the inverse from the cache if the inverse has already been calculated
##
## Description: Return a matrix that is the inverse of 'x' (Compute or Retrieve from cache)

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
        if(!is.null(inv)) {
	        message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
##
## Test Run
## > x = rbind(c(1, 1/2), c(1/2, 1))
## > i = makeCacheMatrix(x)
## Matrix output
## > i$get()
##       [,1] [,2]
## [1,]  1.0  0.5
## [2,]  0.5  1.0
## First run - Compute inverse of matrix
## > cacheSolve(i)
## 	[,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333
## Second run - Retrieve inverse of matrix from cache
## > cacheSolve(i)
## getting cached data
## 	[,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333
