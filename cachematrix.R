## Put comments here that give an overall description of what your
## functions do

## Create a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  ##define function to set the value of the matrix, it also clears the old inverse from the cache
  set <- function(y){
    x <<- y  #set the value 
    m <<- NULL  #clear the cache 
  }
  ##define function to get the value of matrix 
  get <- function() x 
  ##define function to set the inverse, only used by getinverse() when there is no cached inverse
  setInverse <- function(inverse) m <<- inverse 
  ##define function to get the inverse 
  getInverse <- function() m 
  ##Return a list wiht the above four functions 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return inverse of matrix x. This function computes the inverse of the special matrix returned by makeCacheMatrix aobe. 
## If the inverse has already been calculated, the cachesolve retrieves the inverse from the cache. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() ##this fetches the cached value for the inverse
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ##the cache was empty, need to calculate, cache, then return it 
    data <- x$get() ##get value of matrix 
    m <- solve(data) ##calculate inverse 
    x$setInverse(m) ##cache the result 
    m ##return the inverse
}
