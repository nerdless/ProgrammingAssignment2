## This code creates two function, the first one make a cached version of a matrix
## and the second one calculate the inverse of that matrix 

## makeCacheMatrix fuction create a chachedd version of a matrix, it is a list containing
## the matrix, and three methods, get the matrix, set the inverse matrix and get the 
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function return the inverse of a cached version of a matrix, it does that
## by getting the cached value of a matrix or otherwise, if there is not a cached value, 
## calculate the inverse matrix with the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
