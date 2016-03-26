## These functions attempt to create matrix of the inputs
## and then takes the inverse of the matrix and returns
## either that newly calculated inverted matrix and stores the result
## matrix in cache. if result is called on again, cache is returned, else
## new calculation returned

## This is the matrix function creators
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # gets the matrix with original values
  get <- function() x
  # this function sets the inverse
  setmatrix <- function(matrix) m <<- matrix 
  # this function gets the solved matrix
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
  # check to see if cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #return value if cached
  }
  data <- x$get()
  m <- solve(data, ...) # pass through solve function for inverse to value
  x$setmatrix(m)        # set matrix with newly set value
  m
}
