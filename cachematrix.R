## These functions attempt to create matrix of the inputs
## and then takes the inverse (1/x) of the matrix and returns
## either that newly calculated inverted matrix and stores the result
## matrix in cache. if result is called on again, cache is returned, else
## new calculation returned

## This is the matrix function creator
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # checks to see if inputed is vector or matrix
  get <- function() x
  if (is.matrix(x)) {
    #if matrix, set the inverse of the matrix
    setmatrix <- function(matrix) m <<- 1/x 
  } else {
    #if not matrix, convert to matrix and set its invert
    setmatrix <- function(matrix) m <<- apply(matrix(x), c(1,2), function(x) 1/x) 
  }
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Checks to see if solved variable is in cache or not
## if in cache, returns cached matrix, else returns newly calculated
## inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- x$setmatrix(data)
  m
}
