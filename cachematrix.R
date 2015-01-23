# makeCacheMatrix: This function creates a special "matrix" 
# object that can cache its inverse.
#   parameter x: the matrix to solve
#   variable  m: the internal cache
# methods:
#   get() - retrieve value
#   set(value) - set value
#   getsolve() - read inverse from cache
#   setsolve(value) - write inverse to cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# compute the inverse of the special "matrix" 
# that is returned by makeCacheMatrix
#  parameter x: the "special" matrix to solve
#  variable  m: the return value, to contain the inverse of the matrix
# if there already exists an inverse for matrix x (and x has not been changed), 
# this value is returned. 
# Otherwise a new inverse is calculated by the solve function and then cached in x
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inversed matrix")
    return(m)
  } 
  data <- x$get()
  m <- solve(data, ...)
  message(cat("store new matrix ", m))
  x$setsolve(m)
  m
}



