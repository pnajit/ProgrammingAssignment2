## Assignment: Caching the Inverse of a Matrix
## Decription: Matrix inversion is usually a costly computation and we will get some benefit if we can cache 
##             the inverse of a matrix rather than compute it repeatedly. The following code has functions 
##             which will cache the inverse of a matrix.


# The first function, makeCacheMatrix creates a special "matrix", which is really a list 
# containing a function to
# 1.This function creates a special "matrix" object that can cache its inverse.
# 2.set the value of the matrix
# 3.get the value of the matrix
# 4.set the value of the Matrix Inverve
# 5.get the value of the Matrix Inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatInverse <- function(solve) m <<- solve
  getMatInverse <- function() m
  list(set = set, get = get,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of matrix returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Assumption: Only Invertible matrix is passed

cacheSolve <- function(x, ...){
  
  m <- x$getMatInverse()
  if(!is.null(m)) {
    message("getting cached Inverse Matrix data")
    ## Return a matrix that is the inverse of 'x' from cache
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatInverse(m)
  ## Return a matrix that is the inverse of 'x' not from cache
  return(m)
}

