## Author - Vishal Sharma
## Date - 15/12/2014


## The function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(objMatrix = matrix()) {
  # initialize the Cached inverse value to NULL
  cachedInverseValue <- NULL
  
  # to set the value of the matrix
  set <- function(objNewMatrix) {
    objMatrix <<- objNewMatrix
    cachedInverseValue <<- NULL   # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() objMatrix
  # to set the inverse
  setinv <- function(computedInverseValue) cachedInverseValue <<- computedInverseValue
  # to get the inverse
  getinv <- function() cachedInverseValue
  
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)    
}


## The following function calculates the inverse of the special 
## "matrix" created with the above function. However, it first 
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(objMatrix, ...) {
  # check if the inverse is already cached
  invInCache <- objMatrix$getinv()
  if(!is.null(invInCache)) {
    message("getting cached data")
    return(invInCache)
  }
  # not cached, so get the matrix into matrixData
  matrixData <- objMatrix$get()
  # and compute the inverse
  invComputed <- solve(matrixData, ...)
  # then cache the inverse
  objMatrix$setinv(invComputed)
  # and return it as well
  invComputed
}