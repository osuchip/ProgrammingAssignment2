## Put comments here that give an overall description of what your
## functions do

## The following functions cache the inverse of a matrix and returns the inverse
## from the cache, if present in the cache. Else, inverse of the matrix is computed and
## returned (also stored in the cache).

## makeCacheMatrix sets the value of inverse and gets the value of the inverse
## matrix

makeCacheMatrix <- function(x = matrix()) {

  # inv_matrix stores the cached inverse of the matrix
  # It is intialized to NULL
  inv_matrix <- NULL
  
  ## Set the value of inv matrix in the workspace
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  ## Get the value of inv_matrix
  get <- function() x
  
  ## Set the value of the inverse matrix
  setinverse <- function(inverse) inv_matrix <<- inverse
  ## Get the value of the inverse matrix
  getinverse <- function() inv_matrix
  
  # Return the list created 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computed the inverse of the matrix. If the inverse of the 
## matrix is already in the cache, it retreives the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse
  
  inv_matrix <- x$getinverse()
  
  ## If thet inverse matrix is in the cache, return it
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  ## Else, compute the inverse and store in the cache,and return it at the 
  ## end of the function call
  
  mtrx <- x$get()
  inv_matrix <- solve(mtrx)
  x$setinverse(inv_matrix)
  inv_matrix
}
