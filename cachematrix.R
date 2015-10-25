## Function makeCacheMatrix
##    usage: A1 <- makeCacheMatrix(A)
##       -->> given A as a matrix that is invertible
##       -->> Note that "x" must be a matrix, not a vector
##  
##  The function creates "subfunctions" callable on any item defined as the function.
##     get usage: A1$get()
##        --> returns the value of the matrix A1
##     set usage: A1$set()
##        --> sets the value of the matrix A1
##     getInverse usage: A1$getInverse()
##        --> returns the current value of the cached inverse
##        -->   This will be NULL for the first call, then A1's inverse afterwards
##     setInverse usage: A1$setInverse()
##        --> sets the cached inverse to the inverse of the matrix returned by A1$getM()
##
##  This code is based on the example "makeVector" function
##    provided by the instructor of the R Programming Coursera class by Johns Hopkins.

makeCacheMatrix <- function(x = matrix()) {
  #make sure matrix is invertible, or it will error out.
  if(det(x) == 0) {
    print("Matrix is not invertible.")
    return(NULL)
  }
  
  #Initialize everything.
  i <- NULL
  set <- function(y) {
    x <<- y
    i <- NULL
    #setInverse(x)
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
  
}


## Function cacheSolve
##    usage: cacheSolve(A1)
##      --> given that A1 has been set via makeMatrix already
##    The function returns the inverse of the matrix sent to makeMatrix.
##      --> it will return a cached version, if available.
##      --> Otherwise it will calculate it and cache that value.
##
##  This code is based on the example "cachemean" function 
##    provided by the instructor of the R Programming Coursera class by Johns Hopkins.

cacheSolve <- function(x, ...) {
  #Check for an existing cached inverse
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Fetching the cached inverse of your matrix...")
    return(i)
  }
  #If nothing in the cache, calculate the inverse and cache it for the future
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  
  return(i)  
  
}