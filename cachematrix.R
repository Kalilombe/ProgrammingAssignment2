## Put comments here that give an overall description of what your functions do
## We will write two functions "makeCacheMatrix" and "cacheSolve" that will cache the 
## inverse of a matrix



## Write a short comment describing this function
## The "makeCacheMatrix" will create the special "matrix" object that can cache its 
## inverse for the input ( an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
  x <<- y
  m <<- NULL
 }
 get <- function() x
 setmean <- function(mean) m <<- mean
 getmean <- function() m
 list(set = set, get = get,
      setmean = setmean,
      getmean = getmean)
}


## Write a short comment describing this function
## The cacheSolve function will compute the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
 m <- x$getmean()
 if(!is.null(m)) {
  message("getting cached data")
  return(m)
 }
 data <- x$get()
 m <- mean(data, ...)
 x$setmean(m)
 m
        ## Return a matrix that is the inverse of 'x'
 cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
   message("getting cached result")
   return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv

}
