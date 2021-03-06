## The following two functions were created for caching the inverse of a matrix.
## Further description for each function is given below.

## The first function, makeCacheMatrix, is intended to create a special matrix object that can
## cache its inverse. 

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


## The second function, below, computes the inverse of the special matrix. If the inverse has already
## been calculated before, it will get the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)){
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
