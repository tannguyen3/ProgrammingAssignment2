## This function creates a special "matrix" object that can cache its inverse. 
# The returned matrix will have 4 functions get, set, getInverse, setInverse
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  
  get <- function() x
  set <- function(y){
    x <<- y
    inverseX <<- NULL
  }

  setInverse <- function(inverse) inverseX <<- inverse
  getInverse <- function() inverseX
  
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache and write a message to console.
cacheSolve <- function(x, ...){
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

