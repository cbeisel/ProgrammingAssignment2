## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

##  makeCacheMatrix: creates a special "matrix", which is really a list
##containing a function to:
##  - set the value of the vector
##  - get the value of the vector
##  - set the value of the mean
##  - get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## cacheSolve: calculates the inverse of the special "matrix" created with 
## makeCacheMatrix. It first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     s <- x$getsolve()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setsolve(s)
     s
}
