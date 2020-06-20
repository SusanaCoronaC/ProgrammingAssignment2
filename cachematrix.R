## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# It Contains a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse 
# 2020-06-20
# SLCC

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

## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by  the function makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then `cacheSolve` should retrieve the inverse from the cache.
# The function assumes that the matrix supplied is always invertible, for example:
# z <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# cacheSolve(z)
# w <- makeCacheMatrix(matrix(c(5,1,0,3, -1, 2, 4, 0, -1), nrow=3))
#  cacheSolve(w)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  
  m <- solve(data, ...)
  x$setsolve(m)
  m
}





