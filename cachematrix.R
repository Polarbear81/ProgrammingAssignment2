## The following two function can be used to cache the inverse
## of a matrix. 

# The function makeCacheMatrix creates a list containing the
# functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inversed matrix
# 4. get the value of the inversed matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  	set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The function cacheSolve returns the inverse of a matrix,
# which needs to be square invertible in this case.
# Before calculating the inverse, the function checks, if 
# the inverse has already been calculated. If this is the case,
# it prints out the message "getting cached data." and skips the 
# computation. When the inverse is not cached, it calculates the
# inverse and sets the value in the cache.
 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
  	message("getting cached data.")
  	return(inv)
  	}
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
