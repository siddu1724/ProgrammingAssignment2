# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrixc <- function(x = matrix()) {
  m <- NULL
  
  # set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the vector
  get <- function() x
  
  # set the value of the mean
  setinv <- function(inverse) m <<- inverse
  
  # get the value of the mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated, then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  # checks to see if the inverse of matrix has already been created, if so then gets it from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  # Computing the inverse of a square matrix
  m <- solve(data, ...)
  x$setinv(m)
  m
}