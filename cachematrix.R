## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix stores the value of the matrix within the function, and returns a list with functions
# that basically point to where in the memory the cached matrix is

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
#if the solution is already in the global variable m, then just take this value, otherwise solve to get the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

##Example usage:
x<- matrix(c(1,2,3,4),2,2)
xm <-makeCacheMatrix(x)
cacheSolve(xm) #first time run it will recreate inverse
cacheSolve(xm) #second time it will take values from cache
