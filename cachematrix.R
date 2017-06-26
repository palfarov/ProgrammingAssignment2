## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix function receives a matrix object as a parameter
##it construct functions get, set, getInv and set Inv
##get returns the original matrix
##set updates the original matrix with a new matrix
##setInv computes the inverse matrix and cache the inverse matrix
##getInv returns the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(x) m <<- solve(x)
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
##cacheSolve function receives a matrix as a parameter
##firts it evaluates if the inverse matrix exists as a calculated vaue in the cache
##it is has been calculated it returns the inverse matrix
##if it has not been calculated it gets the matrix and calculates its inverse
##after that it set the inverse matrix in the cache using the setInv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
