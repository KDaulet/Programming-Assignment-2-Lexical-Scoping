# This function creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(A = matrix()) {
  P <- NULL
  set <- function(B) {
    A <<- B
    P <<- NULL
  }
  get <- function() A
  setsolve <- function(solve) P <<- solve
  getsolve <- function() P
  list ( set = set, get = get, setsolve=setsolve, getsolve=getsolve)
  
}


# This function computes the inverse of the matrix
cacheSolve <- function(A, ...) {
  P <- A$getsolve()
  if(!is.null(P)) {
    message("getting cached data")
    return(P)
  }
  
  data <- A$get()
  P <- solve(data,...)
  A$setsolve(P)
  P
}
