## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  if (is.matrix(x)) {
    setmatrix <- function(matrix) m <<- 1/x
  } else {
    setmatrix <- function(matrix) m <<- apply(matrix(x), c(1,2), function(x) 1/x)
  }
  #setmatrix <- function(matrix) m <<- apply(matrix(x), c(1,2), function(x) 1/x)
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #m <- 1/data
  #x$setmatrix(m)
  m <- x$setmatrix(data)
  m
}
