## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializing the inverse property
  inv <- NULL
  
  ## Creating method to set the inverse of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Creating method to get the inverse of the matrix
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {                ##checking whether inverse is null
    message("getting cached data")
    return(inv)                       ##returns inverse value
  }
  
  data <- x$get()
  inv <- solve(data, ...)             ##calculate inverse value
  x$setinverse(inv)
  
  inv                                 ##return a matrix that is the inverse of x
}
