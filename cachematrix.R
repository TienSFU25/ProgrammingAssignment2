## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function() x
  set <- function(newX) {
    x <<- newX
    inverse <<- NULL
  }
  
  getInverse <- function() inverse
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  if(!is.matrix(x)) {
    stop("Argument must be a matrix")
  }
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(is.null(inverse)) {
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
  }
  
  return(inverse)
}
