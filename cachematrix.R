## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() which should be able to set and get the value of a matrix
# then set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() should compute the inverse of the special "matrix" returned by the above
# if the inverse has been calculated and matrix has not changed, then it should retrive 
# inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}