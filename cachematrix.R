## Define two functions able cache and get the inverse of a Matrix

## This function take a matrix as parameter
## and return a list of four function allowing to get/set matrix/inverse

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinv <- function(iv) invM <<- iv
  getinv <- function() invM
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function take a matrix created with makeCacheMatrix as parameter
## and return this inverse cached or compute the inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getinv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setinv(invM)
  invM
}
