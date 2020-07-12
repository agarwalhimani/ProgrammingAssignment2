## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_ma <- NULL
  set <- function(y) {
    x <<- y
    inv_ma <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_ma <<- inverse
  getinverse <- function() inv_ma
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_ma <- x$getinverse()
  if(!is.null(inv_ma)) {
    message("getting cached data")
    return(inv_ma)
  }
  data <- x$get()
  inv_ma <- solve(data, ...)
  x$setinverse(inv_ma)
  inv_ma
}
x <- matrix(1:4, nrow = 2, ncol = 2)
m <- makeCacheMatrix(x)
cacheSolve(m)

