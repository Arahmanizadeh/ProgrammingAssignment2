##  Below are two functions that are used to create a special object that
## stores a matrix and cache's its inverse.


## The following function creates a special "matrix", 
## which has a list containing a function to set and get both matrix value and
## its inverse value

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


## The following function calculates the inverse of the special "matrix" 
## created with the above function. But, it first checks to see 
## if the inverse has already been calculated. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
