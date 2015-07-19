## Functions for speeding up inverse calculations of matrix using cache
## First use makeCacheMatrix to make inverse of the matrix cache-able
## use $get $set to access the matrix
## use cacheSolve instead of solve to solve the matrix

## Entry point to first create a cache-able matrix
## use x$get() to get to the underlying matrix
## use x$set(y) to reset the underlying matrix to a new value y
makeCacheMatrix <- function(x = matrix()) {
  s <<- NULL
  get <- function() x
  set <- function(y) {
    s <- NULL
    x <<- y
  }
  getSolve <- function() s
  setSolve <- function(inverse) s <<- inverse
  
  list(
    get = get,
    set = set,
    getSolve = getSolve,
    setSolve = setSolve
  )
}

# apply solve to the given cache-able matrix and also cache the result
# parameters: x is a matrix must be create using 'makeCacheMatrix'
# ... rest of the parameters as described by 'solve'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if (!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}