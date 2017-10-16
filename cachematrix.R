## Cache the inverse of a square matrix
##   calls the solve() function to calculate the
##   inverse of a passed in matrix and caches the
##   result.

## Example usage:
## set.seed(1234)
## r = rnorm(10000)
## dasmatrix = matrix(r, nrow = 10, ncol = 10)
## cachematrix <- makeCacheMatrix(dasmatrix)
## cacheSolve(cachematrix)
## cacheSolve(cachematrix) 
## 

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


## Accepts a makeCacheMatrix object and returns the inverse
##   of the matrix
## See the top of file example to test it.

cacheSolve <- function(x, ...) {
 m <- x$getsolve()

  if(!is.null(m)) {
    message("Cache hit")
    return(m)
  } else {
    message("Cache miss")
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
