## Laia Cabré
## R Programming (Coursera) - Assignment 2 (lexical scoping)
## 2015.02.17

## This function creates a special list which contains a function
# which sets the value of a matrix, gets its value, makes its
# inverse and gets its inverse again if it was already calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}



## This function checks first if the inverse of the matrix
# is in the cache or not. If it is there, it gets the value,
# otherwise, it calculates the inverse with solve()

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
