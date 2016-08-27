## This program will calculate the inverse of a matrix using caching by <<- operator

## This function has 4 methods - setMatrix, getMatrix, setInverse and getInverse

## SetMatrix will assign a value to the matrix passed as parameter, getMatrix will fetch the value of the matrix
## Usage:
## a<-makeCacheMatrix(matrix(c(1,3,2,4),nrow=2,ncol=2))
## a$getMatrix
##          [,1] [,2]
##    [1,]    1    2
##    [2,]    3    4

## SetInverse will store the value of argument in the variable inverse
## SetInverse uses the <<- operator to cache the value

## GetInverse will return the value of variable inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function()
  {
    x
  }
  setInverse <- function(solve)
  {
    inverse <<- solve
  }
  getInverse <- function() 
  {
    inverse
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This function will compute and return the cached value of inverse of the matrix

## We first check if the value is already computed using x$getInverse function, if yes then it gets printed.
## If not, we calculate it using solve function and call the setInverse function to set the cache of the inverse for next time

##cacheSolve(a)
##         [,1] [,2]
##    [1,] -2.0  1.0
##    [2,]  1.5 -0.5

## For next use,
## cacheSolve(a)
##getting cached data
##     [,1] [,2]
##  [1,] -2.0  1.0
##  [2,]  1.5 -0.5
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse<- solve(data)
  x$setInverse(inverse)
  inverse
}
