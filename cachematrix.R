## Assignment file for Week 3 of R Programming Cousera course.
## Use below code to test the functions
# A <- matrix( c(5, 1, 0,
#                3,-1, 2,
#                4, 0,-1), nrow=3, byrow=TRUE)
# B <- matrix(rnorm(1:20), nrow=4, byrow = TRUE)
# cacheMatrix <- makeCacheMatrix(A)
# cacheMatrix$inverse() #first time call so the inverse will be calculated
# cacheMatrix$inverse() #second time call so will have the result from the cache - inverseX
# cacheMatrix <- cacheMatrix$set(B) # set new value for the matrix 
# cacheMatrix$inverse() #first time call after set the new so the inverse will be calculated
# cacheMatrix$inverse() #second time call so will have the result from the cache - inverseX

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  
  get <- function() x
  set <- function(y){
    x <<- y
    inverseX <<- NULL
  }

  inverse <- function() {
    if (is.null(inverseX)) {
      inverseX <<- solve(x)
    }
    inverseX
  }
  
  list(get = get, set = set, inverse = inverse)
}

