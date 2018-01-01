## Two functions that cache the inverse of a 
## 2X2 matrix for future use

## This prepares the "sets" and "gets" for the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<-solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This actually calulates the inverse, 
## if there isn't an inverse already cached.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
