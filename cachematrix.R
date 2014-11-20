## Programming Assignment 2 of the R Programming course on Coursera
## https://class.coursera.org/rprog-009/

## makekCacheMatrix creates a setter/getter object that stores a matrix

makeCacheMatrix <- function(x = matrix(),...) {
  print(environment())
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve takes a getter/setter object that contains a matrix, and returns the inverse of that matrix.
## It first attempts to retrieve a calculated inverse from cache.
## if it finds it it returns it.
## If it can't find the inverse in cache, it calculates it and stores it in cache, and returns it
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}

## a test program to demonstrate that the returned inverse is indeed the inverse of the input matrix
## I multiply both matrices and visually inspect that the result is an identity matrix

#myDimensions <- 1000
#temp <- matrix(rpois(myDimensions*myDimensions,5), nrow=myDimensions, ncol=myDimensions)
#myMat <- makeCacheMatrix(temp)
#cacheSolve(myMat)
#zapsmall(temp %*% cacheSolve(myMat))