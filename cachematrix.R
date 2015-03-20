##These fuctions help in calculating the inverse of a non singular square matrix 
##They also help speeding this process by using cache to store the result of 
##the previously calculated inverse

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ##"<<-" operator is used to assign a value to an object in an environment 
    ## that is different from the current environment
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  ## Following list containes matrix and its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
