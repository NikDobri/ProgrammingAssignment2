## Load MSSS package to use ginv(A) function to do generalized inverse of a matrix
library(MASS) 

## This script provides two fuctions. The first function returns a special vector of functions
## to store a matrix and its inverse. It also caches the inverse of the matrix. 
## The second function looks for the inverse of the matrix by first checking if the inverse 
## is in the cache, and if not, it calculate the inverse using the first function

## The following function:
## *sets the value of the matrix
## *gets the value of the matrix
## *sets the value of the inverse
## *gets the value of the inverse
## example of how to use the function:
## 1. Load the function
## 2. Assign it to an object
## mcm <- makeCacheMatrix()
## 3. create the matrix
## > x <- matrix(1:4,nrow=2)
## > x
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## 4. call the special vector of functions
## > mcm$set(x)
## > mcm$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > mcm$setinv(ginv(x))
## > mcm$getinv()
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
         		set <- function(y) {
            	x <<- y
               inv <<- NULL
        		}
        		get <- function() x
       		setinv <- function(inverse) inv <<-inverse
        		getinv <- function() inv
       		list(set = set, get = get,
            	setinv = setinv,
            	getinv = getinv)

}


## The following function calculates the inverse of a matrix using the first function
## however it first looks for the inverse in the cache and if it finds it reports that 
## it did find it in the cache and retreaved it from there
## example of how to use the function
## 1. Load the function
## 2. Execute the function, with an argument being the previous special vector of fucntions
## > cacheSolve(mcm)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## for square matrix
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

## for any matrix
cacheGinv <- function(x, ...) {
       		inv <- x$getinv()
       	 if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        	}
       	 data <- x$get()
       	 inv <- ginv(data, ...)
       	 x$setinv(inv)
       	 inv
}
