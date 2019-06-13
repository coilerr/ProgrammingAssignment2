## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # the set function stores the matrix to the cache
    x <<- y
    m <<- NULL
  }
  get <- function() x #function to use the matrix we want to compute the inverse of
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m #
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  if(!is.null(m)) { #if the value of m is NULL then the inverse matrix was not calculated yet
    message("getting cached data")
    return(m)
  }
  data <- x$get() #a new variable data is created to store the value of the matrix to compute the inverse of
  m <- solve(data, ...) #we compute the inverted matrix of the matrix x =data
  x$setinvmatrix(m) #the newly calculated matrix is stored in cache
  m
}
  ## Return a matrix that is the inverse of 'x'
