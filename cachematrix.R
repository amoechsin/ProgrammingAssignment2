## These functions attempt to create matrix of the inputs
## and then takes the inverse of the matrix and returns
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
<<<<<<< HEAD
  # gets the matrix with original values
  get <- function() x
  # this function sets the inverse
  setmatrix <- function(matrix) m <<- matrix 
  # this function gets the solved matrix
=======
  get <- function() x
  if (is.matrix(x)) {
    setmatrix <- function(matrix) m <<- 1/x
  } else {
    setmatrix <- function(matrix) m <<- apply(matrix(x), c(1,2), function(x) 1/x)
  }
  #setmatrix <- function(matrix) m <<- apply(matrix(x), c(1,2), function(x) 1/x)
>>>>>>> 3c246edab588ab276d25c09e051112989877fcf9
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Checks to see if solved variable is in cache or not
## if in cache, returns cached matrix, else returns newly calculated
## inverted matrix

cacheSolve <- function(x, ...) {
<<<<<<< HEAD
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  # check to see if cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #return value if cached
  }
  data <- x$get()
  m <- solve(data, ...) # pass through solve function for inverse to value
  x$setmatrix(m)        # set matrix with newly set value
=======
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
>>>>>>> 3c246edab588ab276d25c09e051112989877fcf9
  m
}
