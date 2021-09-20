## makeCacheMatrix is a function consisting of set, get, setInv, and getInv.
## library(MASS) allows for the solving of non-square matrices.

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInv <- function(inverse) {inv <<- inverse}
  getInv <- function(){
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve first checks if inverse has already been calculated
## so it can retrieve it from cache and skip the computation.
## If not already calculated, it calculates the inverse
## and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("Gettin' dat ca$h money oops I mean cached matrix!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}