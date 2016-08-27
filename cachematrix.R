## Put comments here that give an overall description of what your
## functions do. CacheMatrix Produces a matrix which cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
## Initialize invr as a litmus paper test for tagging absence or presence in cache
    invr <- NULL
    set <- function(z) {
    x <<- z
    invr <<- NULL
  }
  get <- function() x
  setinvrse <- function(invrse) invr <<- invrse
  getinvrse <- function() invr
  list(set = set,
       get = get,
       setinvrse = setinvrse,
       getinvrse = getinvrse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    invr <- x$getinvrse()
    if (!is.null(invr)) {
      message("get cached data")
      return(invr)
    }
    
    matrx <- x$get()
    invr <- solve(matrx, ...)
    x$setinvrse(invr)
    invr
  
}
