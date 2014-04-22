## Put comments here that give an overall description of what your
## functions do

## This function gets a matrix as an input, set the value of the matrix,
# sets the inverse Matrix and gets the inverse Matrix. 
# The matrix object can cache its own object. 

# <<- operator is used to assign a value to an object in an env than the current env

#take the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it , 
# it returns invertible matrix using cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){       ## if the inv Matrix is not null condition
    message("getting cached data")
    return(j)
  }
  mat <- x$get()         ## get the original matrix
  j <- solve(mat,...)    ## using solve to inverse the matrix
  x$setInverse(j)        ## setting invertible matrix
  j                      ## returning the invertible matrix
}  
