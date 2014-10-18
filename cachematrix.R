#
# Functions to create a special object that stores 
# a numeric matrix and cache's its inverse.
#


## The 'CacheMatrix' constructor
# returns an object with the following methods
#  set : sets the content of the matrix
#  get : returns the content of the matrix
#  setInverse : sets the inverse of the matrix
#  getInverse : gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  # variable to store the cached inverse
  i <- NULL
  
  # to set the matrix data
  set <- function (m) {
    x <<- m
    
    # invalidate the cache if the matrix is modified
    i <<- NULL
  }
  
  # to get the matrix data
  get <- function () x
  
  # to set the inverse of the matrix
  setInverse <- function (inverse) i <<- inverse
  
  # to get the inverse of the matrix
  getInverse <- function () i
  
  # return the CacheMatrix object with its methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function to calculate the inverse of the matrix
# If the inverse has never been calculated
#    it calculates it and caches it
# If it has aleady been calculated
#    it returns the result from the cache

cacheSolve <- function(m, ...) {
        i <- m$getInverse()
        
        if (!is.null(i)) {
          # the cache exists
          message("getting cached data")
        } else {
          # the cache doesn't exist, so we get the data
          data <- m$get()
          
          # calcultate the inverse
          i <- solve(data)
          
          # cache it 
          m$setInverse(i)
        }
        
        # return the inverse
        i
}
