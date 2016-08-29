## Functions to store a matrix and cache the inverse matrix
## Use makeCacheMatrix to store a matrix object that can cache its inverse
## cacheSolve will compute the inverse or return a cached version if available

## creates a list item that contains the original matrix
## and allows the inverse to be computed and stored
## -set() sets the matrix
## -get() accesses the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns the inverse of a matrix by:
## -computing it the first time, and
## -returning a cached value on subsequent calls

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

### use the below for testing ------------------------
# set.seed(123)
# myMatrix <- matrix(floor(runif(10000,0,11)),100,100)
# myMatrixCache <- makeCacheMatrix(myMatrix)
# 
# myMatrixCache$get()
# cacheSolve(myMatrixCache)
# cacheSolve(myMatrixCache)
