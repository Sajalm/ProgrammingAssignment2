## The function makeCacheMatrix creates a special
## Matrix Object, which is really is list that 
## contains a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
## set the value of the matrix    
  inverse <- NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }

## get the value of the matrix  
  get <- function () x
  
## set the value of the inverse
  setinverse <- function(solve) inverse <<- solve

## get the value of the inverse
getinverse <- function() inverse
  
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}

## The function cacheSolve returns the inverse of the 
## special matrix created using makeCacheMatrix.
## It first checks to see if the inverse has already 
## been computed. If so, it gets the inverse from the
## cache and skips computation of inverse.
## Else, it computes the inverse and sets the value of 
## the value of the inverse in the cache via setinverse 
## function.


cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()

## Check if inverse is existing
  if (!is.null(inverse)) {
    print ("Inverse already existing in Cache")
    return (inverse)
  }
  
## Else compute the inverse and return
  data <- x$get()
  inverse <- solve(data, ...)

## set the inverse of the matrix
  x$setinverse(inverse)
  inverse 
}
