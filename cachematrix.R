## R function that is able to cache potentially time-consuming computations when 
## diagonalising large matrices that can be recalled from memory rather than recalculated
## thereby saving time
##
## Created on 14.11.2016 by PB


## Function to store a 'special' matrix in the cache using command:
## e.g. a <- makeCacheMatrix(matrix(1:4,2,2))
##
## Object contains:
## 1. to set the value of the matrix which we want to solve x
##    e.g. a$set(matrix(4:7,2,2)), will recalculate the inverse in cacheSolve, if used
## 2. to retrieve the matrix x
## 3. operators to calculate the inverse of x
## 4. operators to retrieve the solution of the inverse of x

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


## Check if the matrix x already exists and if not calculate
## the inverse of it. x must be defined using makeCacheMatrix first

cacheSolve <- function(x) {
  ## Return the inverse of matrix 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("This function has elements stored in cache")
    message("... getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}


