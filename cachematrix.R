## Programming Assignment 2
##Write a pair of functions that can cache the inverse of a matrix

## Function that creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y){
    x <<- y
    minverse <<- NULL
  } 

get <- function() x
setminverse <- function(solve) minverse <<- solve
getminverse <- function() minverse
list(set = set, get = get, setminverse = setminverse, getminverse = getminverse)
print(x)
}

##Function that computes inverse of matrix created in makeCacheMatrix
cacheSolve <- function(x,...){
##Returns a matrix that is the inverse of 'x'
  minverse <- x$getminverse()
  if(!is.null(minverse)){
    message("Getting cached matrix inverse")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setminverse(minverse)
  minverse
}
