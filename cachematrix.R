## makeCacheMatrix is a function that creates a list of a function that sets
## the value of the matrix, gets the value of the matrix, sets the value
## of inverse matrix, and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of a matrix has previously been 
## calculated. If it has, then the function returns the cached value. If it
## has not, then the function finds the inverse of the matrix, returns it,
## and places it in the cache for later use.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
