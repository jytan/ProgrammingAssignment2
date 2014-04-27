## Functions to cache the calculated inverse of a matrix
## By Junyan Tan 23/04/2014 for R Programming Peer Assessment
## Based on makeVector and cachemean functions by Roger Peng

## Instructions on using the makeCacheMatrix and cacheSolve functions
## 1. Run the command 'y <- makeCacheMatrix(x)' in the R prompt where
##    x is the square invertible matrix one wants to find the inverse of
## 2. Run the command 'cacheSolve(y)' in the R prompt where
##    y is the output from the 1st step. The first time the command is run,
##    the cache is created. Subsequent times will draw the inverse from cache.
## You can create as many cached matrices (and their inverses) as you like
## with the two step process above - just name the output of the 1st step
## with a different variable name

## Instructions on using the qcacheSolve function
## The qcacheSolve function is short for quick cacheSolve
## Only allows a single matrix (and its inverse) to be cached
## but only requires one command 'qcacheSolve(x)' where x is the matrix
## for which the inverse is to be found (compared to the cacheSolve function
## where the input is the output if the makeCacheMatrix function)



## makeCacheMatrix
## This function creates a special "matrix" object that can cache  
## its inverse by creating a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## This function calculates the inverse of the matrix in the 
## list created by the previous function. However, it first checks if the list
## already contains a stored computed inverse, if it does, then the function just 
## calls the stored inverse from the list. Otherwise, it computes the inverse
## of the matrix and sets the value of the inverse of the matrix via the setinverse
## function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## qcacheSolve
## This function is the quick one step version of cacheSolve
## with the limitation of only 1 stored matrix (and its inverse) in the global 
## variable 'cache'. It checks if the matrix of which the inverse is to be found
## is the same as the matrix stored in the cache by a two step process of checking
## the dimensions first then the contents of the matrix. If it is the same,
## the the inverse is drawn directly from the 'cache' variable. Otherwise, the 
## makeCacheMatrix function is called to create the 'cache' variable and the 
## inverse of the matrix is computed and stored using the setinverse function.
qcacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  if (exists("cache")){
    if (all(dim(x)==dim(cache$get()))){
      if (all(x==cache$get())){
        message("getting cached data")
        return(cache$getinverse())
      } 
    }
  }
  cache <<- makeCacheMatrix(x)
  data <- cache$get()
  inv <- solve(data, ...)
  cache$setinverse(inv)
  inv
}
