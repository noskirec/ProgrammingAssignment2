
## Creates a special matrix-like object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(j) {
    x <<- j
    inv <- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv<<- solve
  getmatrix <- function() inv
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Computes the inverse of the matrix-like object returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix hasn't changed),
## it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getmatrix()
  if (!is.null(inv)){
    ## Optional message if matrix is cached
    ## Message("Retrieving cached matrix.")
    return(inv)
  }
  plMatr <- x$get()
  inv <- solve(matr, ...)
  x$setmatrix <- inv
  inv
}
