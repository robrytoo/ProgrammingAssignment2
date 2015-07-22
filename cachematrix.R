## Put comments here that give an overall description of what your
## functions do
  ## These two functions create and allow manipulation of a "matrix" object that can cache its inverse.

## Write a short comment describing this function
  ## The function makeCacheMatrix() initializes the "matrix" object and generates the functions
  ## that manipulate the "matrix" object.
     
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invx) inv <<- invx
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
  ## The function cacheSolve() inverts the "matrix" object returned by makeCacheMatrix()
  ## If "matrix" has been previously changed it inverts the "matrix" by calling solve()
  ## and if "matrix" has not been changed since the last call of this function it will 
  ## return the cahced inverse.
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
