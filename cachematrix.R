## Put comments here that give an overall description of what your
## functions do

## create an empty matrix "x", set() to reset value of Inv upon new creation of a matrix
## store the value of x in get(), setInv() retrieves the value of inversed matrix, and getInv() returns this value

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y){
    x<<- y
    Inv <<- NULL
  }
  get <- function()x
  setInv <- function(inverse) Inv <<- inverse
  getInv <- function() Inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## check the value of Inv, if it is not empty then return cached data, otherwise lookup the value of the matrix from the parent environment and then solve the inverse problem
cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
  if(!is.null(Inv)){
    message("getting cached data")
    return (Inv)
  }
  data <- x$ get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
        ## Return a matrix that is the inverse of 'x'
}
