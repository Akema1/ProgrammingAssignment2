
#makeCacheMatrix creates a list of 4 functions to store a matrix and a cached value of it's inverse
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(solve) {m <<- solve}
  getInverse <- function() {m}
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)

}


# cacheSolve calculates the inverse of a "special" matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
