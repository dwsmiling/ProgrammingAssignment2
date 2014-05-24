## These functions will cache the inverse of a matrix. 
## makeCacheMatrix will cache the data and  the inverted matrix. 
## cacheSolve will compute the inverse of the matrix provided by makeCacheMatrix


## Write a short comment describing this function
## Caches a matrix. Provides the following methods:
## setcache: save to memory cache
## getcache: retrieve from memory cache
## set: set the data matrix (clears cache)
## get: get the data matrix 
makeCacheMatrix <- function(x = matrix()) {

  cache <- NULL
  setDataMatrix <- function(data) {
    x <<- data
    cache <<- NULL
  }
  getDataMatrix <- function() x
  setcache <- function(m) cache <<- m
  getcache <- function() cache
  list(set = setDataMatrix, get = getDataMatrix,
       setcache = setcache,
       getcache = getcache)
}


## Write a short comment describing this function
## x = makeCacheMatrix(x). 
## inverts a matrix using a makeCacheMatrix 
## to cache results. Returns inverted matrix 
## assumes x is invertible
cacheSolve <- function(x, ...) {
       
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
        
}
