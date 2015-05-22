## The two functions below are used to create a special object that stores
## a matrix and caches its inverse

## The makeCacheMatrix function creates a special "matrix" consisting of a
## list of four functions:
## 1. The set function sets the value of the matrix
## 2. The get functino gets the value of the matrix
## 3. The setinverse function sets the value of the inverse
## 4. The getinverse function gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix"
## created with the above function. If the inverse has already been
## calculated, it gets the inverse from the cache.  Otherwise, it 
## calculates the inverse and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
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
