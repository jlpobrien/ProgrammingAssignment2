## Put comments here that give an overall description of what your
## functions do

##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
##Functions will cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse. 

##It will:

##1. Set the value of the matrix
##2. Get the value of the matrix
##3. Set the value of the inverse
##4. Get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. Without the former the latter is incomplete. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

##TEST
x<-matrix(1:4,2,2)
PleaseInverseThisMatrix <- makeCacheMatrix(x)
cacheSolve(PleaseInverseThisMatrix)
