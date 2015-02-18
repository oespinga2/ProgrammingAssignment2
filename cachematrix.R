## These functions create functions that define a 'CacheMatrix' object which allocates space for a matrix and its inverse. A second function computes the inverse of the function or returns the cached matrix

## The functions sets, gets a matrix and it's inverse. Note that it doesn't check for rows or columns or whether the stores matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function returns the cached inverse if this exists or calculates the inverse and stores it in a 'makeCacheMatrix' object if necessary.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
