## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) invx <<- Inverse
  getinverse<- function() invx
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse= getinverse)
  
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# 

cacheSolve <- function(x, ...) {
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  #For this assignment, assume that the matrix supplied is always invertible.
  #Otherwise we should test.
  invx <- solve(data, ...)
  x$setinverse(invx)
  invx
}



#Test with diag(3)
# ma <- diag(3)
# ma
# maa <- makeCacheMatrix(ma)
# maa$get()
# maa$getinverse()
# maa$setinverse()
# cacheSolve(maa)
# maa$getinverse()

