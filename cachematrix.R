## Put comments here that give an overall description of what your
## functions do

## Create structure with get-set params
makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) i <<- inverse
  get.inverse <- function() i
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

## check determinant, set inverse if has not been set, return inverse 
cacheSolve <- function(x, ...) {
  i <- x$get.inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  if(det(x$get()) != 0){
    x$set.inverse(solve(x$get(), ...))
    return(x$get.inverse())
  }
  return(NA)
}
