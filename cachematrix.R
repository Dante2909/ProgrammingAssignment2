## A pair of functions where one caches the inverse of a matrix
## and the other either computes the inverse from the cache if it had been calculated already or calculates it 


## Cashes reverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function () {inv}
  list (set = set,
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## computes reverse matrix or returnes the matrix from cache (if it didn't change)

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
