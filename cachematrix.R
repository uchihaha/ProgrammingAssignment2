##The functions that are created in this exercise will store and return the inverse of a matrix  
##First I want to create a function, "makeCacheMatrix", that sets and gets value of matrix 'x' and also sets and gets its inverse
##then another function, "cacheSolve", will use the first function to return a matrix that is the inverse of 'x'

## This function will set and get the value as well as inverse of matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
      inv_m <- NULL
    set <- function(y) {
      x <<- y
      inv_m <<- NULL
    }
    get <- function() x
    set_inv <- function(solve) inv_m <<- solve
    get_inv <- function() inv_m
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

## This function will return the inverse of 'x' 

cacheSolve <- function(x, ...) {
  inv_m <- x$get_inv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  matr <- x$get()
  inv_m <- solve(matr, ...)
  x$set_inv(inv_m)
  inv_m
}

