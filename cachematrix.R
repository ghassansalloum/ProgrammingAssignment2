## Programming Assignment 2 of the R Programming course on Coursera
## https://class.coursera.org/rprog-009/

## makekCacheMatrix creates a setter/getter object that stores a matrix object

makeCacheMatrix <- function(x = matrix(),...) {
  
  i <- NULL
  
  # x is the matrix object. When 'get' is called, simply return x
  get <- function() x  
  
  # "setinverse" is used to save a value (the inverse of the matrix) in "i"
  # i is the object that stores the inverse passed
  setinverse <- function(inverse) i <<- inverse 
  
  # "getinverse" returns the content of "i", ideally the inverse of matrix x was stored in it.
  getinverse <- function() i
  
  # Return the follow list when makeCacheMatrix is called
  # These are setter/getter methods that this function supports
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
  
  #Assume that x is a matrix created using makeCacheMatrix
  # Try reading the inverse of the matrix stored in x, if available
  i <- x$getinverse()
  
  # if i exists, that's the cached value. Return it!
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if i doesn't exists, compute the inverse of the matrix 
  data <- x$get()
  i <- solve(data, ...)
  
  # Save the computed inverse inside the makeCacheMatrix object 
  x$setinverse(i)
  
  # Return the inverse
  i
  
}

## a test program to demonstrate that the returned inverse is indeed the inverse of the input matrix
## I multiply both matrices and visually inspect that the result is an identity matrix

#myDimensions <- 1000
#temp <- matrix(rpois(myDimensions*myDimensions,5), nrow=myDimensions, ncol=myDimensions)
#myMat <- makeCacheMatrix(temp)
#cacheSolve(myMat)
#m <- zapsmall(temp %*% cacheSolve(myMat))