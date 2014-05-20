## The following functions save computation time when calculating
## the inverse of a matrix by calculating it once and caching
## the inverse along with the matrix in a special data type.

## makeCacheVector creates a special "matrix", which is really a 
## list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks 
## to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse  of the 
## data and sets the value of the inverse in the cache via 
## the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(i)
  m
}
